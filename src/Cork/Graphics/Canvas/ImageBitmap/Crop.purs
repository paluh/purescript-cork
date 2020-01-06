module Cork.Graphics.Canvas.ImageBitmap.Crop where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (new') as CanvasElement
import Cork.Graphics.Canvas.CanvasElement (setMinCanvasPhysicalDimensions)
import Cork.Graphics.Canvas.ImageBitmap (ImageBitmap)
import Cork.Graphics.Canvas.ImageBitmap (dimensions, toCanvasImageSource) as ImageBitmap
import Cork.Graphics.Canvas.ImageBitmap.Types (fromAnyImageData)
import Cork.Graphics.Canvas.ImageData (AnyImageData(..))
import Cork.Graphics.Canvas.ImageData (Mutable) as ImageData
import Cork.Graphics.Canvas.ImageData.Mutable (getImageData)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Geometry.Plane (BoundingBox)
import Graphics.Canvas (CanvasElement, drawImage, getContext2D)
import Seegee.Geometry.Distance.Units (Pixel) as Units

-- | XXX: Crop should take maximum from BoundingBox and dimensions
-- | XXX: Use square canvas with max dimensions

-- | We should move this to `ImageData/Clip.purs`
-- | and optimize - we can use `putImageData(x, y, dx, dy, dw, dh)`
-- | XXX: Use square canvas with max dimensions
cropToMutableImageData ∷ Maybe CanvasElement → BoundingBox Units.Pixel → ImageBitmap → Effect ImageData.Mutable
cropToMutableImageData possibleCanvas bb img = do
  let
    d = (ImageBitmap.dimensions img)
  canvas ← maybe (CanvasElement.new' (ImageBitmap.dimensions img)) (\c → setMinCanvasPhysicalDimensions c d *> pure c) possibleCanvas
  ctx ← getContext2D canvas
  drawImage ctx (ImageBitmap.toCanvasImageSource img) 0.0 0.0
  getImageData ctx bb

crop ∷ Maybe CanvasElement → BoundingBox Units.Pixel → ImageBitmap → Aff ImageBitmap
crop possibleCanvas bb img = do
  let
    d = (ImageBitmap.dimensions img)
  { canvas, mutable } ← liftEffect do
    canvas ← maybe (CanvasElement.new' (ImageBitmap.dimensions img)) (\c → setMinCanvasPhysicalDimensions c d *> pure c) possibleCanvas
    ctx ← getContext2D canvas
    drawImage ctx (ImageBitmap.toCanvasImageSource img) 0.0 0.0
    { canvas, mutable: _ } <$> getImageData ctx bb
  fromAnyImageData canvas (Mutable mutable)

