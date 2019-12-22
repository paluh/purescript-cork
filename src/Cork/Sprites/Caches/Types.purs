module Cork.Sprites.Caches.Types where

import Cork.Graphics.Canvas.ImageBitmap (ImageBitmap)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Symbol (SProxy(..))
import Graphics.Canvas (ImageData)

type Hash = Int

data Item a
  = ProcessingError String
  | Generated a

type Cache a = Map Hash (Item a)

type Caches =
  { imageData ∷ Cache ImageData
  , imageBitmap ∷ Cache ImageBitmap
  }

_imageData = SProxy ∷ SProxy "imageData"

imageDataL ∷ Lens' Caches (Cache ImageData)
imageDataL = prop _imageData

_imageBitmap = SProxy ∷ SProxy "imageBitmap"

imageBitmapL ∷ Lens' Caches (Cache ImageBitmap)
imageBitmapL = prop _imageBitmap
