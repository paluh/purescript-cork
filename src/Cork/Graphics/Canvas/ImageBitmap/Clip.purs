module Cork.Graphics.Canvas.ImageBitmap.Clip where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (setMinCanvasPhysicalDimensions)
import Cork.Graphics.Canvas.Context2D (FillRule(..), clearRect, clipPath)
import Cork.Graphics.Canvas.ImageBitmap.Draw (drawImageFull)
import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap, fromAnyImageData, height, width)
import Cork.Graphics.Canvas.ImageBitmap.Types (dimensions, toCanvasImageSource) as ImageBitmap
import Cork.Graphics.Canvas.ImageBitmap.Types (height', width, width') as Types
import Cork.Graphics.Canvas.ImageData (AnyImageData(..))
import Cork.Graphics.Canvas.ImageData (Mutable) as ImageData
import Cork.Graphics.Canvas.ImageData.Mutable (getImageData, getImageData')
import Cork.Graphics.Canvas.ImageData.Mutable.Types (unsafeFreeze) as ImageData.Mutable.Types
import Cork.Graphics.Canvas.Path2D (Path2D)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Geometry.Plane (BoundingBox(..))
import Geometry.Plane.BoundingBox (dimensions) as BoundingBox
import Graphics.Canvas (CanvasElement, ImageData, drawImage, getContext2D, translate)
import Graphics.Canvas (clearRect, restore, save) as Canvas
import Seegee.Geometry.Distance.Units (Pixel) as Units

clipToMutableImageData ∷ CanvasElement → Path2D → (Maybe (BoundingBox Units.Pixel)) → ImageBitmap → Effect ImageData.Mutable
clipToMutableImageData canvas path bb img = do
  let
    boundingBox@(BoundingBox boundingBoxRecord) = case bb of
      Just b → b
      Nothing → BoundingBox
        { x: 0.0, y: 0.0, width: width img, height: height img }
    dimensions = BoundingBox.dimensions boundingBox
  setMinCanvasPhysicalDimensions canvas dimensions
  ctx ← getContext2D canvas

  Canvas.save ctx
  translate ctx { translateX: -boundingBoxRecord.x, translateY: -boundingBoxRecord.y }
  clearRect ctx boundingBox
  clipPath ctx path NonZero
  drawImageFull ctx img boundingBox boundingBox
  imageData ← getImageData ctx (BoundingBox { x: 0.0, y: 0.0, height: dimensions.height, width: dimensions.width })
  Canvas.restore ctx

  pure imageData

clipToImageData ∷ CanvasElement → Path2D → (Maybe (BoundingBox Units.Pixel)) → ImageBitmap → Effect ImageData
clipToImageData c p bb i = clipToMutableImageData c p bb i >>= ImageData.Mutable.Types.unsafeFreeze

clip ∷ CanvasElement → Path2D → (Maybe (BoundingBox Units.Pixel)) → ImageBitmap → Aff ImageBitmap
clip canvas path bb img = do
  mutable ← liftEffect $ clipToMutableImageData canvas path bb img
  fromAnyImageData canvas (Mutable mutable)

