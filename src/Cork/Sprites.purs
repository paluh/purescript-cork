module Cork.Sprites where

import Prelude

import Cork.App (Config)
import Cork.App (make) as Cork.App
import Cork.Graphics.Canvas.ImageBitmap (ImageBitmap)
import Cork.Graphics.Canvas.ImageBitmap (toCanvasImageSource) as ImageBitmap
import Cork.Render (Draw, Render, Context) as Render
import Cork.Render.Types (DrawCanvasImageSourceF(..), DrawF(..), DrawImageDataF(..))
import Cork.Sprites.Caches (Cache, Caches, imageBitmapL, imageDataL)
import Cork.Sprites.Caches (Item(..)) as Cache
import Cork.Sprites.Caches.Machine (Change(..), Drawing, Drawings)
import Cork.Sprites.Caches.Machine (machine) as Caches.Machine
import Cork.Sprites.Sprite (hash) as Sprite
import Data.Array (catMaybes)
import Data.Bitraversable (bitraverse)
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (ImageData)
import Spork.Interpreter (basicEffect)
import Spork.Interpreter (merge) as Interpreter
import Spork.Transition (Transition, purely)
import Type.Prelude (SProxy(..))

type Scene =
  { above ∷ Drawings
  , below ∷ Drawings
  , workspace ∷ Drawings
  }

type Model =
  { caches ∷ Caches
  , scene ∷ Scene
  }

_caches = SProxy ∷ SProxy "caches"

cachesL ∷ Lens' Model Caches
cachesL = prop _caches

data Action
  = SetImageBitmapCache (Cache ImageBitmap)
  | SetImageDataCache (Cache ImageData)
  | Load Scene

render ∷ Model → Render.Render
render model =
  { above: renderDraws model.scene.above
  , below: renderDraws model.scene.below
  , workspace: renderDraws model.scene.workspace
  }
  where
    renderDraws = catMaybes <<< map (renderDraw model.caches)

renderDraw ∷ Caches → Drawing → Maybe Render.Draw
renderDraw caches (DrawF e) = DrawF <$> (bitraverse renderPutImageData renderDrawImage e)
  where
    renderPutImageData (PutImageData point sprite) =
      (PutImageData point) <$> spriteImageData sprite

    renderDrawImage (DrawImage point sprite) =
      (DrawImage point) <$> spriteBitmap sprite
    renderDrawImage (DrawImageScale bb sprite) =
      (DrawImageScale bb) <$> spriteBitmap sprite

    spriteBitmap sprite = case Map.lookup hash caches.imageBitmap of
      Just (Cache.Generated imageBitmap) → Just
        { hash
        , canvasImageSource: ImageBitmap.toCanvasImageSource imageBitmap
        }
      -- | `Failed`
      Just _ → Nothing
      Nothing → Nothing
      where
        hash = Sprite.hash sprite

    spriteImageData sprite = case Map.lookup hash caches.imageData of
      Just (Cache.Generated imageData) → Just
        { hash
        , imageData
        }
      -- | `Failed`
      Just _ → Nothing
      Nothing → Nothing
      where
        hash = Sprite.hash sprite

update ∷ Render.Context → Model → Action → Transition Effect Model Action
update renderContext model = case _ of
  SetImageBitmapCache imageBitmap →
    purely (set (cachesL <<< imageBitmapL) imageBitmap model)
  SetImageDataCache imageData →
    purely (set (cachesL <<< imageDataL) imageData model)
  Load scene →
    purely (model { scene = scene })

machine ∷ Config → Effect { load ∷ Scene → Effect Unit, run ∷ Effect Unit }
machine config = do
  let
    app =
      { render
      , subs: const mempty
      , update
      , init:
        { model: mempty
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
  let
    load scene = do
      cachesMachine.load (scene.above <> scene.below <> scene.workspace)
      i.push (Load scene)
  pure
    { load
    , run: i.run
    }

