module Cork.Graphics.Canvas.ImageData.Immutable.Types where

import Prelude

import Geometry.Distance (Distance)
import Geometry.Distance (unsafeFromInt) as Distance
import Geometry.Plane (Dimensions)
import Graphics.Canvas (ImageData, imageDataHeight, imageDataWidth)
import Seegee.Geometry.Distance.Units (Pixel) as Units
import Unsafe.Coerce (unsafeCoerce)

type Immutable = ImageData

height ∷ ImageData → Distance Units.Pixel
height = Distance.unsafeFromInt <<< imageDataHeight

width ∷ ImageData → Distance Units.Pixel
width = Distance.unsafeFromInt <<< imageDataWidth

dimensions ∷ ImageData → Dimensions Units.Pixel
dimensions img = unsafeCoerce
  { height: imageDataHeight img, width: imageDataWidth img }
