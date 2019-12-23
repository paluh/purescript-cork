module Cork.Graphics.Canvas.CanvasElement where

import Prelude

import Effect (Effect)
import Geometry.Distance (Distance(..))
import Geometry.Distance (convert, toNumber) as Distance
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Plane (Dimensions) as Geometry.Plane
import Graphics.Canvas (CanvasElement, CanvasImageSource, getCanvasDimensions, setCanvasDimensions)
import Graphics.Canvas (Dimensions) as Canvas
import Seegee.DevicePixelRatio (DevicePixelRatio)
import Seegee.DevicePixelRatio (conversionFactor) as DevicePixelRatio
import Seegee.Geometry.Distance.Units (Pixel, Screen) as Units
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (setAttribute)
import Web.HTML (HTMLCanvasElement)
import Web.HTML.HTMLCanvasElement (toHTMLElement) as HTMLCanvasElement
import Web.HTML.HTMLElement (toElement) as HTMLElement

type Dimensions =
  { physical ∷ Geometry.Plane.Dimensions Units.Pixel
  , logical ∷ Geometry.Plane.Dimensions Units.Screen
  }

fillDimensions ∷ DevicePixelRatio → Geometry.Plane.Dimensions Units.Screen → Dimensions
fillDimensions dpr logical@({ height, width }) =
  let
    cf = DevicePixelRatio.conversionFactor dpr
  in
    { logical
    , physical:
        { height: Distance.convert cf height
        , width: Distance.convert cf width
        }
    }

setPhysicalDimensions ∷ Geometry.Plane.Dimensions Units.Pixel → CanvasElement → Effect Unit
setPhysicalDimensions dimensions c = do
  setCanvasDimensions c (toCanvasElementDimensions dimensions)
  where
    toCanvasElementDimensions ∷ Geometry.Plane.Dimensions Units.Pixel → Canvas.Dimensions
    toCanvasElementDimensions = unsafeCoerce

-- | Resize canvas only when necessary
setMinCanvasPhysicalDimensions ∷ CanvasElement → Geometry.Plane.Dimensions Units.Pixel → Effect Unit
setMinCanvasPhysicalDimensions canvasElement dimensions =  do
  let
    { height: Distance (NonNegative height), width: Distance (NonNegative width) } = dimensions
  canvasDimensions ← getCanvasDimensions canvasElement
  when (canvasDimensions.width < width || canvasDimensions.height < height) do
    setCanvasDimensions
      canvasElement
      { width: max width canvasDimensions.width
      , height: max height canvasDimensions.height
      }

setLogicalDimensions ∷ Geometry.Plane.Dimensions Units.Screen → CanvasElement → Effect Unit
setLogicalDimensions dimensions canvasElement = do
  setAttribute "width" (serDistancePx dimensions.width) elem
  setAttribute "height" (serDistancePx dimensions.height) elem
  where
    elem = HTMLElement.toElement (HTMLCanvasElement.toHTMLElement (toHTMLCanvasElement canvasElement))
    serDistancePx d = (show $ Distance.toNumber d) <> "px"

setDimensions ∷ Dimensions → CanvasElement → Effect Unit
setDimensions dimensions canvasElement = do
  setPhysicalDimensions dimensions.physical canvasElement
  setLogicalDimensions dimensions.logical canvasElement

fromHTMLCanvasElement ∷ HTMLCanvasElement → CanvasElement
fromHTMLCanvasElement = unsafeCoerce

toHTMLCanvasElement ∷ CanvasElement → HTMLCanvasElement
toHTMLCanvasElement = unsafeCoerce

-- | It could be, but we want to speed it up a bit ;-)
-- | `new = window >>= document >>= toDocument >>> createElement "canvas"`
foreign import newImpl ∷ Effect CanvasElement

new ∷ Dimensions → Effect CanvasElement
new dimensions = do
  canvasElement ← newImpl
  setDimensions dimensions canvasElement
  pure canvasElement

new' ∷ Geometry.Plane.Dimensions Units.Pixel → Effect CanvasElement
new' physical =
  let
    logical = unsafeCoerce physical
  in
    new { physical, logical }

foreign import clone ∷ CanvasElement → Effect CanvasElement

toCanvasImageSource ∷ CanvasElement → CanvasImageSource
toCanvasImageSource = unsafeCoerce

