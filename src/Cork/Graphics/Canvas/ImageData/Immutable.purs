module Cork.Graphics.Canvas.ImageData.Immutable
  ( fromHTMLLoadedImageElement
  , fromHTMLLoadedImageElement'
  , height
  , width
  )
  where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (new) as CanvasElement
import Cork.Graphics.Canvas.CanvasElement (setMinCanvasPhysicalDimensions)
import Cork.Web.HTML.HTMLLoadedImageElement (HTMLLoadedImageElement)
import Cork.Web.HTML.HTMLLoadedImageElement (naturalDimensions, toCanvasImageSource) as HTMLLoadedImageElement
import Effect (Effect)
import Geometry (Distance)
import Geometry.Distance (toNumber, unsafeFromInt) as Distance
import Geometry.Distance.ConversionFactor (unsafe) as ConversionFactor
import Geometry.Plane.BoundingBox.Dimensions (convert) as Dimensions
import Graphics.Canvas (CanvasElement, ImageData, drawImage, getContext2D, getImageData, imageDataHeight, imageDataWidth)
import Seegee.Geometry.Distance.Units (Pixel) as Units

height ∷ ImageData → Distance Units.Pixel
height = Distance.unsafeFromInt <<< imageDataHeight

width ∷ ImageData → Distance Units.Pixel
width = Distance.unsafeFromInt <<< imageDataWidth

fromHTMLLoadedImageElement ∷ CanvasElement → HTMLLoadedImageElement → Effect ImageData
fromHTMLLoadedImageElement canvas img = do
  let
    dimensions = HTMLLoadedImageElement.naturalDimensions img
  setMinCanvasPhysicalDimensions canvas dimensions
  context ← getContext2D canvas
  drawImage
    context
    (HTMLLoadedImageElement.toCanvasImageSource img)
    0.0
    0.0

  getImageData
    context
    0.0
    0.0
    (Distance.toNumber dimensions.width)
    (Distance.toNumber dimensions.height)

fromHTMLLoadedImageElement' ∷ HTMLLoadedImageElement → Effect ImageData
fromHTMLLoadedImageElement' img = do
  let
    dimensions = HTMLLoadedImageElement.naturalDimensions img
  canvas ← CanvasElement.new { physical: dimensions, logical: Dimensions.convert (ConversionFactor.unsafe 1.0) dimensions }
  context ← getContext2D canvas
  drawImage
    context
    (HTMLLoadedImageElement.toCanvasImageSource img)
    0.0
    0.0

  getImageData
    context
    0.0
    0.0
    (Distance.toNumber dimensions.width)
    (Distance.toNumber dimensions.height)
