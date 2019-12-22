module Cork.Sprites.Caches.ImageData where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Cork.Data.Array.Builder (build, cons) as Array.Builder
import Cork.Graphics.Canvas.ImageData.Immutable (fromHTMLLoadedImageElement') as ImageData.Immutable
import Cork.Graphics.Canvas.ImageData.Mutable (thaw, unsafeFreeze) as ImageData.Mutable
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale (filter) as Grayscale
import Cork.Machines.SelfFeeding (Machine, Update, make) as SelfFeeding
import Cork.Sprites.Caches (Item) as Caches
import Cork.Sprites.Caches (Item(..))
import Cork.Sprites.Caches.ImageData.Plan (Plan, layerFoldMapWithIndex, plan)
import Cork.Sprites.Caches.ImageData.Plan (step) as Plan
import Cork.Sprites.Caches.Types (Cache, Hash, Item)
import Cork.Sprites.Caches.Types (Hash) as Types
import Cork.Sprites.Sprite (Sprite, SpriteF)
import Cork.Sprites.Sprite.Filters (GrayscaleF(..), StackedBlurF(..), _grayscale, _stackedBlur)
import Cork.Sprites.Sprite.Images (ExternalImageF(..), _externalImage)
import Cork.Web.HTML.HTMLLoadedImageElement (new) as HTMLLoadedImageElement
import Data.Foldable (foldMap)
import Data.Functor.Variant (case_, on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (insert, lookup, singleton) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (ImageData)
import Matryoshka (Algebra, cata)
import Spork.Batch (batch)
import Spork.Interpreter (basicAff)
import Spork.Interpreter (merge, never) as Interpreter

type ImageDataPlan = Plan Aff Types.Hash (Item ImageData)

-- | Raw build up plan without cache usage
spriteImageDataPlanAlg ∷ Algebra SpriteF ((Item ImageData → ImageDataPlan) → ImageDataPlan)
spriteImageDataPlanAlg = case_
  # on _stackedBlur (\(StackedBlurF { hash, radius } build) cont →
    let
      -- process ∷ ImageData → Aff (Item ImageData)
      process imageData = onError (const $ "StackedBlur") $ liftEffect do
        mutable ← ImageData.Mutable.thaw imageData
        Grayscale.filter mutable
        Generated <$> ImageData.Mutable.unsafeFreeze mutable
    in
      build (step hash process cont))
  # on _grayscale (\(GrayscaleF hash build) →
    let
      process imageData = onError (const $ "Grayscale") $ liftEffect do
        mutable ← ImageData.Mutable.thaw imageData
        Grayscale.filter mutable
        Generated <$> ImageData.Mutable.unsafeFreeze mutable
    in
      \cont → build (step hash process cont))
  # on _externalImage (\(ExternalImageF { hash, url }) →
    let
      process = onError (\e → "ExternalImage:" <> unsafeStringify e) do
        img ← HTMLLoadedImageElement.new url
        imageData ← liftEffect $ ImageData.Immutable.fromHTMLLoadedImageElement' img
        pure $ Generated imageData
    in
      \cont → plan (Map.singleton hash (Tuple process cont)))
  where
    step ∷ Hash → (ImageData → Aff (Item ImageData)) → (Item ImageData → ImageDataPlan) → (Item ImageData → ImageDataPlan)
    step hash process cont = case _ of
      (Generated imageData) → plan (Map.singleton hash (Tuple (process imageData) cont))
      failure → plan (Map.singleton hash (Tuple (pure failure) cont))

    onError msg process = process `catchError` \e → pure (ProcessingError (msg e))

type ImageDataCachingM a = ReaderT (Cache ImageData) Aff a
type ImageDataCachingPlan = Plan ImageDataCachingM Types.Hash (Item ImageData)

spriteImageDataCachingPlan ∷ Sprite → ImageDataCachingPlan
spriteImageDataCachingPlan sprite =
  mapWithIndex check imageDataPlan
  where
    imageDataPlan = cata spriteImageDataPlanAlg sprite mempty

    check ∷ Types.Hash → Aff (Item ImageData) → ImageDataCachingM (Item ImageData)
    check hash process = do
      cache ← ask
      case Map.lookup hash cache of
        Just r → pure r
        Nothing → lift process

data Action
  = CacheImageData Hash (Item ImageData)
  | Load (Array Sprite)

type Model =
  { cache ∷ Cache ImageData
  , plan ∷ ImageDataCachingPlan
  }

type Machine = SelfFeeding.Machine Aff Array Model Action

update ∷ SelfFeeding.Update Aff Model Action
update model = case _ of
  Load sprites →
    let
      plan = foldMap spriteImageDataCachingPlan sprites
      effects = layerEffects plan
      x = do
        traceM "LOADING SPRITES"
        traceM sprites
        Nothing
    in
      { model: model { plan = plan }, effects }
  CacheImageData hash item →
    let
      cache = Map.insert hash item model.cache
      { subplan, plan } = Plan.step hash item model.plan
      effects = layerEffects subplan
      x = do
        traceM $ "LOADED SPRITE" <> show hash
        traceM item
        Nothing
    in
      { model: { cache: cache, plan: plan}, effects: effects }
  where
    layerEffects plan =
      let
        step hash process = case Map.lookup hash model.cache of
          Nothing → Array.Builder.cons $ flip runReaderT model.cache do
            item ← process
            pure (CacheImageData hash item)
          Just _ → mempty
      in
        batch $ Array.Builder.build (layerFoldMapWithIndex step plan)

type Change = { new ∷ { hash ∷ Hash, item ∷ Caches.Item ImageData }, cache ∷ Cache ImageData }

machine ∷ Effect
  { process ∷ Array Sprite → Effect Unit
  , subscribe ∷ (Change → Effect Unit) → Effect (Effect Unit)
  }
machine = do
  i ← SelfFeeding.make
    (basicAff  (const $ pure unit) `Interpreter.merge` Interpreter.never)
    { update
    , subs: const $ mempty
    , init: mempty
    }
  let
    subscribe handler = i.subscribe case _ of
      { action: CacheImageData hash item, new: new } -> handler { new: { hash, item }, cache: new.cache }
      _ → pure unit
    process sprites = do
      i.push (Load sprites)
      i.run

  pure { process, subscribe }

