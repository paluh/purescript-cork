module Cork.Sprites.Caches.Types where

import Control.Plus (empty) as Plus
import Cork.Graphics.Canvas.ImageBitmap (ImageBitmap)
import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Symbol (SProxy(..))
import Graphics.Canvas (ImageData)

type Hash = Int

type Item a = Either String a

type Cache a = Map Hash (Item a)

type Caches =
  { imageData ∷ Cache ImageData
  , imageBitmap ∷ Cache ImageBitmap
  }

-- | Till `Map` `Monoid` instance is back it
-- | is convenient to use this value
empty ∷ Caches
empty = { imageData: Plus.empty, imageBitmap: Plus.empty }

_imageData = SProxy ∷ SProxy "imageData"

imageDataL ∷ Lens' Caches (Cache ImageData)
imageDataL = prop _imageData

_imageBitmap = SProxy ∷ SProxy "imageBitmap"

imageBitmapL ∷ Lens' Caches (Cache ImageBitmap)
imageBitmapL = prop _imageBitmap
