module Cork.Sprites.Caches.Machine where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Cork.Data.Array.Builder (build, cons) as Array.Builder
import Cork.Graphics.Canvas (ImageBitmap) as Canvas
import Cork.Graphics.Canvas (TilesNumber(..))
import Cork.Graphics.Canvas.CanvasElement (clone, new') as CanvasElement
import Cork.Graphics.Canvas.ImageBitmap (clipToImageData, fromAnyImageData, fromSource, perspectiveToImageData, toImageData) as ImageBitmap
import Cork.Graphics.Canvas.ImageData (AnyImageData(..))
import Cork.Graphics.Canvas.ImageData.Immutable.Filters.Blur (filter) as Blur
import Cork.Graphics.Canvas.ImageData.Mutable (thaw, unsafeFreeze) as ImageData.Mutable
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale (filter) as Grayscale
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale.ToAlpha (filter) as GrayscaleToAlpha
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.StackedBlur (filter) as StackedBlur
import Cork.Graphics.Canvas.Path2D (new) as Path2D
import Cork.Hashable (Hash)
import Cork.Machines.SelfFeeding (Update, make) as SelfFeeding
import Cork.Render (DrawCanvasImageSourceF(..))
import Cork.Render.Types (DrawF(..), DrawImageDataF(..), DrawingStyle)
import Cork.Sprites.Caches.Plan (Plan(..), layerFoldMapWithIndex)
import Cork.Sprites.Caches.Plan (null, step) as Plan
import Cork.Sprites.Caches.Types (Cache, Item, Caches)
import Cork.Sprites.Sprite (ImageBitmap, ImageBitmapF(..), ImageData, ImageDataF(..)) as Sprite
import Cork.Sprites.Sprite (fromImageData)
import Cork.Svg.Path (print) as Svg.Path
import Cork.Web.HTML.HTMLLoadedImageElement (Source(..))
import Data.Array (catMaybes)
import Data.Array (elem) as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, hush)
import Data.Foldable (foldMap, null)
import Data.Functor.Mu (Mu(..), roll, unroll)
import Data.Hashable (hash)
import Data.Map (insert, lookup, singleton) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.AVar (new) as Effect.AVar
import Effect.Aff (Aff)
import Effect.Aff (bracket) as Aff
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (new, put, take) as AVar.Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (new, read, write) as Ref
import Geometry.Plane.BoundingBox (BoundingBox)
import Geometry.Plane.BoundingBox.Dimensions (unsafe) as Dimensions
import Geometry.Plane.Figures.Polygons.Quadrilateral (Quadrilateral)
import Geometry.Plane.Point (Point)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement)
import Graphics.Canvas (ImageData) as Canvas
import Seegee.Geometry.Distance.Units (Pixel) as Units
import Spork.Batch (batch)
import Spork.Interpreter (basicAff)
import Spork.Interpreter (merge, never) as Interpreter
import Unsafe.Coerce (unsafeCoerce)

type Draw = DrawF Sprite.ImageData Sprite.ImageBitmap

drawImage ∷ Point Units.Pixel → DrawingStyle → Sprite.ImageBitmap → Draw
drawImage point style imageBitmap = DrawF (Right (DrawImage point style imageBitmap))

drawImageData ∷ Point Units.Pixel → DrawingStyle → Sprite.ImageData → Draw
drawImageData p s d = drawImage p s (fromImageData d)

drawImageScale ∷ BoundingBox Units.Pixel → DrawingStyle → Sprite.ImageBitmap → Draw
drawImageScale bb style imageBitmap = DrawF (Right (DrawImageScale bb style imageBitmap))

drawImagePerspective ∷ Quadrilateral Units.Pixel → TilesNumber → DrawingStyle → Sprite.ImageBitmap → Draw
drawImagePerspective quad tilesNumber style imageBitmap = DrawF (Right (DrawImagePerspective quad tilesNumber style imageBitmap))

putImageData ∷ Point Units.Pixel → Sprite.ImageData → Draw
putImageData point imageData = DrawF (Left (PutImageData point imageData))

type Draws = Array Draw

type DrawingPlanBase m imageData imageBitmap = Plan Hash m (Item imageData) (Item imageBitmap)

type UnfoldDrawingPlans m imageData imageBitmap =
  { imageData
    ∷ Sprite.ImageData
    → (Item imageData → DrawingPlanBase m imageData imageBitmap)
    → DrawingPlanBase m imageData imageBitmap
  , imageBitmap
    ∷ Sprite.ImageBitmap
    → (Item imageBitmap → DrawingPlanBase m imageData imageBitmap)
    → DrawingPlanBase m imageData imageBitmap
  }

type Caches' imageData imageBitmap =
  { imageData ∷ Cache imageData
  , imageBitmap ∷ Cache imageBitmap
  }

unfoldDrawPlan
  ∷ ∀ imageData imageBitmap m
  . Caches' imageData imageBitmap
  → UnfoldDrawingPlans m imageData imageBitmap
  → Draw
  → DrawingPlanBase m imageData imageBitmap
unfoldDrawPlan caches interpreters (DrawF d) = case d of
  Right (DrawImage _ _ imageBitmap) → unfoldImageBitmapPlan' imageBitmap
  Right (DrawImageScale _ _ imageBitmap) → unfoldImageBitmapPlan' imageBitmap
  Right (DrawImagePerspective _ _ _ imageBitmap) → unfoldImageBitmapPlan' imageBitmap
  Left (PutImageData _ imageData) → unfoldImageDataPlan' imageData
  where
    unfoldImageBitmapPlan' imageBitmap =
      case Map.lookup (hash imageBitmap) caches.imageBitmap of
        Just item → mempty
        Nothing → interpreters.imageBitmap imageBitmap (const $ mempty)

    unfoldImageDataPlan' imageData@(In i) =
      case Map.lookup (hash i) caches.imageData of
        Just item → mempty
        Nothing → interpreters.imageData imageData (const $ mempty)

type DrawingPlan = DrawingPlanBase Aff Canvas.ImageData Canvas.ImageBitmap

unfoldImageBitmapPlan
  ∷ AVar CanvasElement
  → Sprite.ImageBitmap
  → (Item Canvas.ImageBitmap → DrawingPlan)
  → DrawingPlan
unfoldImageBitmapPlan workspace imageBitmap build = case imageBitmap of
  Sprite.ExternalImage hash url →
    Plan mempty
      (Map.singleton hash (Tuple ib build))
    where
      ib = do
        let
          repr = case url of
            URL u → u
            otherwise → "<source>"
        traceM $ "ExternalImage " <> repr
        r ← (Right <$> ImageBitmap.fromSource url) `catchError` (unsafeStringify >>> Left >>> pure)
        case r of
          Left e → log $ "ERROR" <> e
          Right _ → log $ "Succeeded."
        pure r
  Sprite.FromImageData hash imageData → unfoldImageDataPlan workspace (roll imageData) \i →
    let
      process = do
        Aff.bracket
          (AVar.Aff.take workspace)
          (\c → do
              AVar.Aff.put c workspace
          )
          (\w → do
            traverse (ImageBitmap.fromAnyImageData w) (Immutable <$> i)
          )
    in
      Plan mempty (Map.singleton hash (Tuple process build))

unfoldImageDataPlan
  ∷ AVar CanvasElement
  → Sprite.ImageData
  → (Item Canvas.ImageData → DrawingPlan)
  → DrawingPlan
unfoldImageDataPlan workspace imageData build =
  let
    x = do
      traceM imageData
      Nothing
  in case unroll imageData of
  Sprite.Blur hash b imageData → unfoldImageDataPlan workspace imageData \i →
    let
      process i' = Aff.bracket
        (AVar.Aff.take workspace)
        (flip AVar.Aff.put workspace)
        \c1 → liftEffect $ do
          c2 ← CanvasElement.clone c1
          Right <$> Blur.filter c1 c2 b i'
    in step hash process build i
  Sprite.ClipImageBitmap hash path edgeStyle bb imageBitmap → unfoldImageBitmapPlan workspace imageBitmap \i →
    let
      process i' = Aff.bracket
        (AVar.Aff.take workspace)
        (flip AVar.Aff.put workspace)
        \w → liftEffect $ do
          path2D ← Path2D.new (Svg.Path.print path)
          Right <$> ImageBitmap.clipToImageData w path2D edgeStyle bb i'
    in step hash process build i
  Sprite.FromImageBitmap hash bb imageBitmap → unfoldImageBitmapPlan workspace imageBitmap \i →
    let
      process i' = do
        traceM "FromImageBitmap"
        Aff.bracket
          (AVar.Aff.take workspace)
          (flip AVar.Aff.put workspace)
          (\w → do
            traceM "Succeeded"
            liftEffect $ Right <$> ImageBitmap.toImageData w bb i'
          )
    in step hash process build i
  Sprite.Grayscale hash mode imageData → unfoldImageDataPlan workspace imageData \i →
    let
      onError label = flip catchError (\err → traceM (label <> ".Error :" <> unsafeStringify err) *> throwError err)
      process i' = onError "Grayscale" $ liftEffect do
      -- process i' = liftEffect do
        mutable ← ImageData.Mutable.thaw i'
        Grayscale.filter mode mutable
        Right <$> ImageData.Mutable.unsafeFreeze mutable
    in step hash process build i
  Sprite.GrayscaleToAlpha hash mode imageData → unfoldImageDataPlan workspace imageData \i →
    let
      -- process i' = onError (const $ "GrayscaleToAlpha") $ liftEffect do
      process i' = liftEffect do
        traceM "GrayscaleToAlpha"
        mutable ← ImageData.Mutable.thaw i'
        GrayscaleToAlpha.filter mode mutable
        Right <$> ImageData.Mutable.unsafeFreeze mutable
    in
      step hash process build i
  Sprite.Project hash quad imageBitmap → unfoldImageBitmapPlan workspace imageBitmap \i →
    let
      process i' = Aff.bracket
        (AVar.Aff.take workspace)
        (flip AVar.Aff.put workspace)
        (\w → liftEffect $ Right <$> ImageBitmap.perspectiveToImageData w quad (TilesNumber 5) i')
    in
      step hash process build i
  Sprite.StackedBlur hash radius imageData → unfoldImageDataPlan workspace imageData \i →
    let
      -- process imageData = onError (const $ "StackedBlur") $ liftEffect do
      process i' = liftEffect do
        mutable ← ImageData.Mutable.thaw i'
        StackedBlur.filter mutable radius
        Right <$> ImageData.Mutable.unsafeFreeze mutable
    in step hash process build i
  where
    step ∷ ∀ i. Hash → (i → Aff (Item Canvas.ImageData)) → _ → Item i → _
    step hash process cont = case _ of
      (Right imageData) →
        let
          x = do
            traceM "ImageData step succeeded."
            Nothing
        in
          Plan (Map.singleton hash (Tuple (process imageData) cont)) mempty
      (Left err) →
        let
          x = do
            traceM "Step error"
            traceM err
            Nothing
        in
          Plan (Map.singleton hash (Tuple (pure (Left err)) cont)) mempty

data Action
  = CacheImageData Hash (Item Canvas.ImageData)
  | CacheImageBitmap Hash (Item Canvas.ImageBitmap)
  | Load Draws

type Model =
  { caches ∷ Caches
  , plan ∷ DrawingPlan
  , workspace ∷ AVar CanvasElement
  }

update ∷ SelfFeeding.Update Aff Model Action
update model = case _ of
  Load draws →
    let
      u =
        { imageData: unfoldImageDataPlan model.workspace
        , imageBitmap: unfoldImageBitmapPlan model.workspace
        }

      plan = foldMap (unfoldDrawPlan model.caches u) draws
      effects = layerEffects plan
    in
      { model: model { plan = plan }, effects }
  CacheImageData hash item →
    let
      imageData = Map.insert hash item model.caches.imageData
      { subplan, plan } = Plan.step hash (Left item) model.plan
      effects = layerEffects subplan
      x = do
        traceM "New image data"
        traceM hash
        Nothing
    in
      { model:
        { caches: model.caches { imageData = imageData }
        , plan: plan
        , workspace: model.workspace
        }
      , effects: effects
      }
  CacheImageBitmap hash item →
    let
      imageBitmap = Map.insert hash item model.caches.imageBitmap
      { subplan, plan } = Plan.step hash (Right item) model.plan
      effects = layerEffects subplan
      x = do
        traceM "New image bitmap"
        traceM hash
        Nothing
    in
      { model:
        { caches: model.caches { imageBitmap = imageBitmap }
        , plan: plan
        , workspace: model.workspace
        }
      , effects: effects
      }

  where
    layerEffects plan =
      let
        stepImageData hash process =
          Array.Builder.cons $ do
            traceM "Processing image data..."
            item ← process `catchError` \e → do
              traceM $ "Processing image data error: " <> unsafeStringify e
              throwError e
            traceM "Processing image data succeeded."
            pure (CacheImageData hash item)
        stepImageBitmap hash process =
          Array.Builder.cons $ do
            item ← process
            pure (CacheImageBitmap hash item)
      in
        batch $ Array.Builder.build (layerFoldMapWithIndex stepImageData stepImageBitmap plan)

type CacheChange sprite =
  { new ∷ { hash ∷ Hash, item ∷ Item sprite }
  , cache ∷ Cache sprite
  }

data Change
  = ImageDataChange (CacheChange Canvas.ImageData)
  | ImageBitmapChange (CacheChange Canvas.ImageBitmap)
  | Done


type Machine =
  { process ∷ Draws → Effect Unit
  , subscribe ∷ (Change → Effect Unit) → Effect (Effect Unit)
  }

machine ∷ Effect Machine
machine = do
  canvas ← CanvasElement.new' $ Dimensions.unsafe { height: 1000.0, width: 1000.0 }
  workspace ← Effect.AVar.new canvas

  i ← SelfFeeding.make
    (basicAff  (const $ pure unit) `Interpreter.merge` Interpreter.never)
    { update
    , subs: const $ mempty
    , init:
      { effects: mempty
      , model:
        { caches: mempty
        , plan: mempty
        , workspace
        }
      }
    }
  let
    subscribe handler = i.subscribe case _ of
      { action: CacheImageBitmap hash item, new } →
        let
          change = ImageBitmapChange
            { new: { hash, item }, cache: new.caches.imageBitmap }
        in do
          handler change
          when (Plan.null new.plan) (handler Done)
      { action: CacheImageData hash item, new } →
        let
          change = ImageDataChange
              { new: { hash, item }, cache: new.caches.imageData }
        in do
          handler change
          when (Plan.null new.plan) (handler Done)
      { action: Load _ , new } → do
        when (Plan.null new.plan) (handler Done)

    process draws = do
      i.push (Load draws)
      i.run

  pure { process, subscribe }

