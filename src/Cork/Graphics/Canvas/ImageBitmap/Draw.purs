module Cork.Graphics.Canvas.ImageBitmap.Draw
  ( drawImageFull
  )
  where

import Prelude

import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap)
import Effect (Effect)
import Geometry.Plane (BoundingBox)
import Graphics.Canvas (Context2D)
import Seegee.Geometry.Distance.Units (Pixel) as Units

-- XXX: We want to move all `draw*` functions from `ImageBitmap.purs` module here
-- so we can use them internally here.
-- This module should only depend on `ImageBitmap.Types` and provide canvas compatibile
-- methods with our primitives (like `BoundingBox`, `Distance`, `ImageBitmap` instead of
-- `CanvasSource` etc.)

foreign import drawImageFull ∷ Context2D → ImageBitmap → BoundingBox Units.Pixel → BoundingBox Units.Pixel → Effect Unit
