module Cork.Graphics.Canvas.ImageBitmap.Clip where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (setMinCanvasPhysicalDimensions)
import Cork.Graphics.Canvas.Context2D (FillRule(..), clearRect, clipPath)
import Cork.Graphics.Canvas.ImageBitmap.Draw (drawImageFull)
import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap, fromAnyImageData, height, width)
import Cork.Graphics.Canvas.ImageData (AnyImageData(..))
import Cork.Graphics.Canvas.ImageData (Mutable) as ImageData
import Cork.Graphics.Canvas.ImageData.Mutable (getImageData)
import Cork.Graphics.Canvas.ImageData.Mutable.Types (unsafeFreeze) as ImageData.Mutable.Types
import Cork.Graphics.Canvas.Path2D (Path2D)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Geometry.Distance (toNumber, unsafe) as Distance
import Geometry.Plane (BoundingBox(..))
import Geometry.Plane.BoundingBox (dimensions) as BoundingBox
import Graphics.Canvas (CanvasElement, ImageData, getContext2D, translate)
import Graphics.Canvas (restore, save) as Canvas
import Seegee.Geometry.Distance.Units (Pixel) as Units

-- | I'm not able to find the source of firefox blurring and darkening on edges.
-- | This is an escape hatch.
foreign import crispy ∷ { originalData :: ImageData.Mutable, imageData ∷ ImageData.Mutable } → Effect Unit

data EdgeStyle = Crispy | Antialiased
derive instance eqEdgeStyle ∷ Eq EdgeStyle

clipToMutableImageData ∷ CanvasElement → Path2D → EdgeStyle → (Maybe (BoundingBox Units.Pixel)) → ImageBitmap → Effect ImageData.Mutable
clipToMutableImageData canvas path edgeStyle bb img = do
  let
    boundingBox@(BoundingBox boundingBoxRecord) = case bb of
      Just b → b
      Nothing → BoundingBox
        { x: 0.0, y: 0.0, width: Distance.unsafe $ Distance.toNumber (width img) + 2.0, height: Distance.unsafe $ Distance.toNumber (height img) + 2.0 }
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

  when (edgeStyle == Crispy) $ do
    Canvas.save ctx
    translate ctx { translateX: -boundingBoxRecord.x, translateY: -boundingBoxRecord.y }
    drawImageFull ctx img boundingBox boundingBox
    originalData ← getImageData ctx (BoundingBox { x: 0.0, y: 0.0, height: dimensions.height, width: dimensions.width })
    crispy { originalData, imageData }
    Canvas.restore ctx

  pure imageData

clipToImageData ∷ CanvasElement → Path2D → EdgeStyle → (Maybe (BoundingBox Units.Pixel)) → ImageBitmap → Effect ImageData
clipToImageData c p e bb i = clipToMutableImageData c p e bb i >>= ImageData.Mutable.Types.unsafeFreeze

clip ∷ CanvasElement → Path2D → EdgeStyle → (Maybe (BoundingBox Units.Pixel)) → ImageBitmap → Aff ImageBitmap
clip canvas path e bb img = do
  mutable ← liftEffect $ clipToMutableImageData canvas path e bb img
  fromAnyImageData canvas (Mutable mutable)

