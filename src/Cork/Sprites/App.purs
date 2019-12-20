module Cork.Sprites.App where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (runExcept, runExceptT)
import Control.Monad.Reader (runReader, runReaderT)
import Cork.App (App)
import Cork.Graphics.Canvas.CanvasElement (new) as CanvasElement
import Cork.Graphics.Canvas.ImageBitmap (fromAnyImageData, toCanvasImageSource) as ImageBitmap
import Cork.Graphics.Canvas.ImageData.Types (AnyImageData(..)) as ImageData
import Cork.Render (Draw, Render, Context) as Render
import Cork.Render.Types (DrawCanvasImageSourceF(..))
import Cork.Sprites.App.Types (CacheAction, CacheActionF, CacheImageBitmapF(..), CacheImageDataF(..), Draw, Render, State, _cacheImageBitmap, _cacheImageData, cacheImageBitmap, cacheImageData, cachesL)
import Cork.Sprites.Cache (Cache, _imageData, imageBitmapL, imageDataL)
import Cork.Sprites.Cache (Item(..)) as Cache
import Cork.Sprites.Sprite (hash) as Sprite
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, length)
import Data.Functor.Variant (case_, on)
import Data.Lens (over)
import Data.Map (insert, lookup, singleton) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over) as Newtype
import Data.Traversable (traverse)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Geometry.Plane.BoundingBox.Dimensions (unsafe) as Dimensions
import Global.Unsafe (unsafeStringify)
import Matryoshka (cata, cataM)
import Spork.Batch (batch)
import Spork.Transition (Transition)

-- data Action
--   = LoadSprites (Array Sprite)
--   
-- 
-- model =
--   { 

-- drawBuildPlans ∷ Cache → Draw → BuildPlans
-- drawBuildPlans cache = case _ of
--   DrawImage point sprite →  spriteBuildPlans sprite
--   DrawImageScale point sprite → spriteBuildPlans sprite
--   where
--     spriteBuildPlans sprite = case Map.lookup hash cache.imageBitmap of
--       Just _ → mempty
--       Nothing →
--         { imageData: imageDataBuildPlan'
--         , imageBitmap: Map.singleton hash (ImageData.Immutable >>> ImageBitmap.fromAnyImageData)
--         }
--       where
--         hash = Sprite.hash sprite
--         imageDataBuildPlan = cata (imageDataBuildPlanAlg sprite) (const $ mempty)
--         imageDataBuildPlan' = Newtype.over (map checkCache) imageDataBuildPlan
--           where
--           checkCache producer = case cache of
--             Just imageData → pure imageData
--             Nothing → producer

      --   Left err → mempty
      --   Right result → case result of
      --     (ImageData _) → Nothing
      --     Generating → Nothing
      --     (GenerateImageData hash effect) → Just do
      --       item ← (map Cache.Generated $ effect) `catchError` \e →
      --         pure (Cache.Failed $ Cache.ProcessingError $ unsafeStringify e)
      --       pure (cacheImageData hash item)
      --     (GenerateImageBitmap hash effect) → Just do
      --       item ← (map Cache.Generated $ effect) `catchError` \e →
      --         pure (Cache.Failed $ Cache.ProcessingError $ unsafeStringify e)
      --       pure (cacheImageBitmap hash item)

-- renderDraw ∷ ∀ ctx. ImageDataAlgCtx ctx → Draw → Maybe Render.Draw
-- renderDraw ctx = case _ of
--   DrawImage point sprite →
--     (DrawImage point) <$> spriteBitmap sprite
--   DrawImageScale bb sprite →
--     (DrawImageScale bb) <$> spriteBitmap sprite
--   where
--     spriteBitmap sprite = case Map.lookup hash ctx.cache.imageBitmap of
--       Just (Cache.Generated imageBitmap) → Just
--         { hash
--         , canvasImageSource: ImageBitmap.toCanvasImageSource imageBitmap
--         }
--       -- | `Generating` or `Failed`
--       Just _ → Nothing
--       Nothing → Nothing
--       where
--         hash = Sprite.hash sprite
-- 
-- render ∷ ∀ st. State st → Render.Render
-- render state =
--   { above: renderDraws state.render.above
--   , below: renderDraws state.render.below
--   , workspace: renderDraws state.render.workspace
--   }
--   where
--     renderDraws = catMaybes <<< map (renderDraw state)
-- 
-- 
-- 
-- renderEffects ∷ ∀ ctx. ImageDataAlgCtx ctx → Render → Array (Aff (CacheAction ()))
-- renderEffects ctx r = catMaybes <<< foldMap (map (drawEffects ctx)) $ [ r.above, r.below, r.workspace ]
-- 
-- update ∷ ∀ action st. Render.Context → State st → CacheAction () → Transition Aff (State st) (CacheAction ())
-- update _ model = cata alg
--   where
--     -- x = do
--     --   traceM model.cache.imageData
--     --   traceM (length model.cache.imageData ∷ Int)
-- 
--     --   traceM model.cache.imageBitmap
--     --   traceM (length model.cache.imageBitmap ∷ Int)
--     --   Nothing
-- 
--     alg = case_
--       # on _cacheImageData (\(CacheImageDataF { hash, imageData }) →
--         let
--           model' = over (cacheL <<< imageDataL) (Map.insert hash imageData) model
--           effects = batch (renderEffects model' model'.render)
--         in
--           { model: model', effects: mempty })
--       # on _cacheImageBitmap (\(CacheImageBitmapF { hash, imageBitmap }) →
--         let
--           model' = over (cacheL <<< imageBitmapL) (Map.insert hash imageBitmap) model
--           effects = batch (renderEffects model' model'.render)
--         in
--           { model: model', effects: mempty })
-- 
-- app ∷ Render → Effect (App _ _ _ _)
-- app r = do
--   let
--     raw = { height: 100.0, width: 100.0 }
--     dimensions =
--       { physical: Dimensions.unsafe raw
--       , logical: Dimensions.unsafe raw
--       }
--   canvasElement ← CanvasElement.new dimensions
--   let
--     model =
--       { cache: mempty
--       , render: r
--       , workspace: canvasElement
--       }
--     effects = batch $ renderEffects model r
--   pure
--     { render
--     , update
--     , subs: const mempty
--     , init:
--       { model
--       , effects
--       }
--     }
-- 
