module Cork.Sprites.Caches.Machine where

import Prelude

import Cork.Data.Array.Builder (build, cons) as Array.Builder
import Cork.Render.Types (DrawF)
import Cork.Sprites.Caches.ImageBitmap (Change, machine) as Caches.ImageBitmap
import Cork.Sprites.Caches.ImageData (Change, machine) as Caches.ImageData
import Cork.Sprites.Sprite (Sprite)
import Cork.Sprites.Sprite (hash) as Sprite
import Data.Array (catMaybes)
import Data.Array (elem) as Array
import Data.Bifunctor (bimap)
import Data.Either (either, hush)
import Data.Foldable (foldMap)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Ref (new, read, write) as Ref

type Drawing = DrawF Sprite Sprite

type Drawings = Array Drawing

data Change
  = ImageDataChange Caches.ImageData.Change
  | ImageBitmapChange Caches.ImageBitmap.Change

machine ∷ Effect
  { subscribe ∷ (Change → Effect Unit) → Effect (Effect Unit)
  , load ∷ Drawings → Effect Unit
  }
machine = do
  imageBitmap ← Caches.ImageBitmap.machine
  imageData ← Caches.ImageData.machine
  state ← Ref.new []

  void $ imageData.subscribe \{ new: { hash, item }, cache } → do
    canvasSources ← Ref.read state
    when (hash `Array.elem` canvasSources) $
      imageBitmap.process hash item

  let
    load drawings = do
      let
        canvasSources = hashes <<< catMaybes <<< map (hush <<< unwrap) $ drawings
          where
            hashes = Array.Builder.build <<< foldMap (foldMap (Array.Builder.cons <<< Sprite.hash))

        sprites = Array.Builder.build $
          foldMap (either identity identity <<< bimap (foldMap Array.Builder.cons) (foldMap Array.Builder.cons) <<< unwrap) drawings
      Ref.write canvasSources state
      imageData.process sprites

    subscribe sub = do
      unB ← imageBitmap.subscribe (\c → sub (ImageBitmapChange c))
      unD ← imageData.subscribe (\c → sub (ImageDataChange c))
      pure $ do
        unB
        unD
  pure
    { subscribe
    , load
    }

