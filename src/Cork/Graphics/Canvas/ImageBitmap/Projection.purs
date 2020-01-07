module Cork.Graphics.Canvas.ImageBitmap.Projection where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (setMinCanvasPhysicalDimensions)
import Cork.Graphics.Canvas.Context2D (TilesNumber, clearRect)
import Cork.Graphics.Canvas.ImageBitmap.Draw (drawImagePerspective)
import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap) as Types
import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap, fromAnyImageData)
import Cork.Graphics.Canvas.ImageData (AnyImageData(..), Mutable) as ImageData
import Cork.Graphics.Canvas.ImageData.Mutable (getImageData)
import Cork.Graphics.Canvas.ImageData.Mutable.Types (unsafeFreeze) as ImageData.Mutable.Types
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Geometry.Plane.BoundingBox (BoundingBox(..))
import Geometry.Plane.BoundingBox (dimensions) as BoundingBox
import Geometry.Plane.Figures.Polygons.Quadrilateral (Quadrilateral)
import Geometry.Plane.Figures.Polygons.Quadrilateral (boundingBox) as Quadrilateral
import Graphics.Canvas (CanvasElement, ImageData, getContext2D, translate)
import Graphics.Canvas (restore, save) as Canvas
import Seegee.Geometry.Distance.Units (Pixel) as Units

perspectiveToMutableImageData ∷ CanvasElement → Quadrilateral Units.Pixel → TilesNumber → Types.ImageBitmap  → Effect ImageData.Mutable
perspectiveToMutableImageData canvas quad tilesNumber img = do
  let
    boundingBox@(BoundingBox boundingBoxRecord) = Quadrilateral.boundingBox quad
    dimensions = BoundingBox.dimensions boundingBox

  setMinCanvasPhysicalDimensions canvas dimensions
  ctx ← getContext2D canvas

  Canvas.save ctx

  translate ctx { translateX: -boundingBoxRecord.x, translateY: -boundingBoxRecord.y }
  clearRect ctx boundingBox
  drawImagePerspective ctx img quad tilesNumber
  imageData ← getImageData ctx (BoundingBox { x: 0.0, y: 0.0, height: dimensions.height, width: dimensions.width })

  Canvas.restore ctx

  pure imageData

perspectiveToImageData ∷ CanvasElement → Quadrilateral Units.Pixel → TilesNumber → Types.ImageBitmap → Effect ImageData
perspectiveToImageData c q t i = perspectiveToMutableImageData c q t i >>= ImageData.Mutable.Types.unsafeFreeze

perspective ∷ CanvasElement → Quadrilateral Units.Pixel → TilesNumber → Types.ImageBitmap → Aff ImageBitmap
perspective canvas quad tilesNumber img = do
  mutable ← liftEffect $ perspectiveToMutableImageData canvas quad tilesNumber img
  fromAnyImageData canvas (ImageData.Mutable mutable)
