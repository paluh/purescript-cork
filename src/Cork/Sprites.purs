module Cork.Sprites
  ( module Exports
  , machine
  , Machine
  , Scene
  )
  where

import Prelude

import Cork.App (Config)
import Cork.App (make) as Cork.App
import Cork.Graphics.Canvas.CanvasElement (Dimensions) as CanvasElement
import Cork.Graphics.Canvas.ImageBitmap (ImageBitmap)
import Cork.Render (Draw, Render, Context) as Render
import Cork.Render.Types (DrawCanvasImageSourceF(..), DrawF(..), DrawImageDataF(..))
import Cork.Render.Zoom (Zoom)
import Cork.Sprites.Caches (Cache, Caches, imageBitmapL, imageDataL)
import Cork.Sprites.Caches.Machine (Change(..), Draw, Draws)
import Cork.Sprites.Caches.Machine (Change(..), drawImage, drawImageData, drawImagePerspective, drawImageScale, Draw, Draws, putImageData) as Exports
import Cork.Sprites.Caches.Machine (machine) as Caches.Machine
import Data.Array (catMaybes)
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..))
import Data.Functor.Mu (Mu(..))
import Data.Hashable (hash)
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (ImageData)
import Record (insert) as Record
import Spork.Interpreter (basicEffect)
import Spork.Interpreter (merge) as Interpreter
import Spork.Transition (Transition, purely)
import Type.Prelude (SProxy(..))
import Web.HTML (HTMLElement)

type Scene =
  { above ∷ Draws
  , below ∷ Draws
  , workspace ∷ Draws
  }

type Model =
  { caches ∷ Caches
  , dimensions ∷ CanvasElement.Dimensions
  , done ∷ Maybe (Scene → Effect Unit)
  , scene ∷ Scene
  }

_caches = SProxy ∷ SProxy "caches"

cachesL ∷ Lens' Model Caches
cachesL = prop _caches

data Action
  = SetImageBitmapCache (Cache ImageBitmap)
  | SetImageDataCache (Cache ImageData)
  | Load { done ∷ Maybe (Scene → Effect Unit), scene ∷ Scene }

render ∷ Model → Render.Render
render model =
  { above: renderDraws model.scene.above
  , below: renderDraws model.scene.below
  , workspace: renderDraws model.scene.workspace
  }
  where
    renderDraws = catMaybes <<< map (renderDraw model.caches)

renderDraw ∷ Caches → Draw → Maybe Render.Draw
renderDraw caches (DrawF e) = DrawF <$> (bitraverse renderPutImageData renderDrawImage e)
  where
    renderPutImageData (PutImageData point sprite) =
      (PutImageData point) <$> spriteImageData sprite
    renderDrawImage (DrawImage point style sprite) =
      (DrawImage point style) <$> spriteBitmap sprite
    renderDrawImage (DrawImageScale bb style sprite) =
      (DrawImageScale bb style) <$> spriteBitmap sprite
    renderDrawImage (DrawImagePerspective quad tilesNumber style sprite) =
      (DrawImagePerspective quad tilesNumber style) <$> spriteBitmap sprite

    spriteBitmap sprite = case Map.lookup h caches.imageBitmap of
      Just (Right imageBitmap) → Just
        { hash: h
        , canvasImageSource: imageBitmap
        }
      -- | `Failed`
      Just _ → do
        -- traceM "FAILURED"
        Nothing
      Nothing → do
        -- traceM $ ((length caches.imageBitmap) ∷ Int)
        -- traceM caches.imageBitmap
        -- traceM $ Array.fromFoldable $ Map.Internal.keys caches.imageData
        -- traceM $ Array.fromFoldable $ Map.Internal.keys caches.imageBitmap
        -- traceM h
        -- traceM "MISSING"
        Nothing
      where
        h = hash sprite

    spriteImageData (In i) = case Map.lookup h caches.imageData of
      Just (Right imageData) → Just
        { hash: h
        , imageData
        }
      -- | `Failed`
      Just _ → Nothing
      Nothing → Nothing
      where
        h = hash i

update ∷ Render.Context → Model → Action → Transition Effect Model Action
update renderContext model = case _ of
  SetImageBitmapCache imageBitmap →
    purely (set (cachesL <<< imageBitmapL) imageBitmap model)
  SetImageDataCache imageData →
    purely (set (cachesL <<< imageDataL) imageData model)
  Load { done, scene } →
    purely (model { done = done, scene = scene })

type Machine =
  { append ∷ HTMLElement → Effect Unit
  , render ∷
      { dimensions ∷ CanvasElement.Dimensions
      , done ∷ Maybe (Scene → Effect Unit)
      , scene ∷ Scene
      , zoom ∷ Zoom
      }
    → Effect Unit
  , run ∷ Effect Unit
  -- , setDimensions ∷ CanvasElement.Dimensions → Effect Unit
  }

machine ∷ Config → Effect Machine
machine config = do
  let
    app =
      { render
      , subs: const mempty
      , update
      , init:
        { model: Record.insert (SProxy ∷ SProxy "dimensions") config.dimensions mempty
        , effects: mempty
        }
      }
  i ← Cork.App.make (basicEffect `Interpreter.merge` basicEffect) app config
  cachesMachine ← Caches.Machine.machine

  void $ cachesMachine.subscribe case _ of
    ImageDataChange change → do
      i.push (SetImageDataCache change.cache)
      i.run

    ImageBitmapChange change → do
      i.push (SetImageBitmapCache change.cache)
      i.run

    Done → i.snapshot >>= \m → case m.done of
      Just callback → callback m.scene
      Nothing → pure unit

  let
    renderScene { dimensions, done, scene, zoom } = do
      -- | We probably don't have to check dimensions change here
      -- | as the same could be done on the app level
      whenM (i.snapshot <#> _.dimensions >>> eq dimensions >>> not) $
        i.setDimensions dimensions
      i.setZoom zoom
      i.push (Load { done, scene })
      i.run
      -- | I should keep cache status
      -- | in state probably and this machine too.
      -- | Then I would be able to
      -- | sync this submachine with state
      -- | transitions easily.
      -- | Now I created this strange order
      -- | so when empty scene is loaded
      -- | we have `done` handler already
      -- | in state...
      cachesMachine.process (scene.above <> scene.below <> scene.workspace)
  pure
    { append: \a → i.append a *> i.run
    , render: renderScene
    , run: i.run
    }

