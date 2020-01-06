module Cork.Graphics.Canvas.ImageData.Mutable.Types where

import Prelude

import Effect (Effect)
import Geometry (Distance)
import Geometry.Plane.BoundingBox (BoundingBox(..))
import Geometry.Plane.BoundingBox.Dimensions (Dimensions)
import Graphics.Canvas (Context2D, ImageData)
import Graphics.Canvas (getImageData) as Canvas
import Seegee.Geometry.Distance.Units (Pixel) as Units
import Unsafe.Coerce (unsafeCoerce)

foreign import data Mutable ∷ Type

getImageData ∷ Context2D → BoundingBox Units.Pixel → Effect Mutable
getImageData ctx (BoundingBox { height: h, width: w, x, y }) =
  coerceGetImageData Canvas.getImageData ctx x y w h
  where
    coerceGetImageData
      ∷ (Context2D → Number → Number → Number → Number → Effect ImageData)
      → (Context2D → Number → Number → Distance Units.Pixel → Distance Units.Pixel → Effect Mutable)
    coerceGetImageData = unsafeCoerce

getImageData' ∷ Context2D → Number → Number → Number → Number → Effect Mutable
getImageData' ctx x y w h = Canvas.getImageData ctx x y w h >>= unsafeThaw

foreign import freeze ∷ Mutable → Effect ImageData

foreign import thaw ∷ ImageData → Effect Mutable

foreign import new ∷ Dimensions Units.Pixel → Effect Mutable

-- | Dimensions of a mutable image data can't change

foreign import height ∷ Mutable → Int

foreign import width ∷ Mutable → Int

dimensions ∷ Mutable → Dimensions Units.Pixel
dimensions img = unsafeCoerce { height: height img, width: width img }

foreign import unsafeFreeze ∷ Mutable → Effect ImageData

foreign import unsafeThaw ∷ ImageData → Effect Mutable

