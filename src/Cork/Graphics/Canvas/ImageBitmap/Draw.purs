module Cork.Graphics.Canvas.ImageBitmap.Draw
  ( drawImageFull
  , drawImagePerspective
  )
  where

import Prelude

import Cork.Graphics.Canvas.Context2D (TilesNumber, drawImagePerspective) as Context2D
import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap)
import Cork.Graphics.Canvas.ImageBitmap.Types (dimensions', toCanvasImageSource) as Types
import Effect (Effect)
import Geometry.Plane (BoundingBox, point)
import Geometry.Plane.Figures.Polygons.Quadrilateral (Quadrilateral)
import Graphics.Canvas (Context2D)
import Seegee.Geometry.Distance.Units (Pixel) as Units

-- XXX: We want to move all `draw*` functions from `ImageBitmap.purs` module here
-- so we can use them internally in ImageBitmap module.
-- This module should only depend on `ImageBitmap.Types` and provide canvas compatibile
-- methods with our primitives (like `BoundingBox`, `Distance`, `ImageBitmap` instead of
-- `CanvasSource` etc.)

foreign import drawImageFull ∷ Context2D → ImageBitmap → BoundingBox Units.Pixel → BoundingBox Units.Pixel → Effect Unit

drawImagePerspective
  ∷ Context2D
  → ImageBitmap
  → Quadrilateral Units.Pixel
  → Context2D.TilesNumber
  → Effect Unit
drawImagePerspective context img dest tilesNumber = do
  let
    { height: h, width: w } = Types.dimensions' img
  Context2D.drawImagePerspective
    context
    (Types.toCanvasImageSource img)
    { first: point 0.0 0.0
    , second: point w 0.0
    , third: point w h
    , fourth: point 0.0 h
    }
    dest
    tilesNumber

