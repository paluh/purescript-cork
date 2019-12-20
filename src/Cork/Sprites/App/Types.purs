module Cork.Sprites.App.Types where

import Prelude

import Cork.Data.Functor.Variant.Mu (MuVariantF, inj)
import Cork.Graphics.Canvas.ImageBitmap (ImageBitmap)
import Cork.Render (DrawCanvasImageSourceF)
import Cork.Sprites.Cache (Cache)
import Cork.Sprites.Cache (Hash, Item) as Cache
import Cork.Sprites.Cache.Types (Caches)
import Cork.Sprites.Sprite (Sprite)
import Data.Functor.Variant (FProxy, VariantF)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Graphics.Canvas (CanvasElement, ImageData)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

type State st =
  { caches ∷ Caches
  , render ∷ Render
  , workspace ∷ CanvasElement
  | st
  }

_caches = SProxy ∷ SProxy "caches"

cachesL ∷ ∀ st. Lens' (State st) Caches
cachesL = prop _caches

type Draw = DrawCanvasImageSourceF Sprite

type Draws = Array Draw

type Render =
  { above ∷ Draws
  , below ∷ Draws
  , workspace ∷ Draws
  }

data CacheImageDataF r = CacheImageDataF { hash ∷ Cache.Hash, imageData ∷ Cache.Item ImageData }
derive instance functorCacheImageDataF ∷ Functor CacheImageDataF

data CacheImageBitmapF r = CacheImageBitmapF { hash ∷ Cache.Hash, imageBitmap ∷ Cache.Item ImageBitmap }
derive instance functorCacheImageBitmapF ∷ Functor CacheImageBitmapF

type CacheActionRow r = (cacheImageData ∷ FProxy CacheImageDataF, cacheImageBitmap ∷ FProxy CacheImageBitmapF | r)

_cacheImageData = SProxy ∷ SProxy "cacheImageData"
_cacheImageBitmap = SProxy ∷ SProxy "cacheImageBitmap"

type CacheActionF = VariantF (CacheActionRow + ())
type CacheAction r = MuVariantF (CacheActionRow + r)

cacheImageData :: forall r. Cache.Hash → Cache.Item ImageData → MuVariantF (CacheActionRow  + r)
cacheImageData hash imageData = inj _cacheImageData (CacheImageDataF { hash, imageData })

cacheImageBitmap :: forall r. Cache.Hash → Cache.Item ImageBitmap  → MuVariantF (CacheActionRow + r)
cacheImageBitmap hash imageBitmap = inj _cacheImageBitmap (CacheImageBitmapF { hash, imageBitmap })

-- | Replace the whole render
-- data LoadF r = LoadF Render
-- derive instance functorCacheImageDataF ∷ Functor CacheImageDataF
