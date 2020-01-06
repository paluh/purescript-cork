module Cork.Graphics.Canvas.CanvasElement
  -- ( module Types
  -- , fillDimensions
  -- , setPhysicalDimensions
  -- , setMinCanvasPhysicalDimensions
  -- )
  where

import Prelude

import Cork.Web.HTML.HTMLElement (setStyleProperty)
import Effect (Effect)
import Geometry.Distance (Distance(..))
import Geometry.Distance (convert, toNumber) as Distance
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Plane (Dimensions) as Geometry.Plane
import Geometry.Plane.BoundingBox.Dimensions (unsafe) as Dimensions
import Graphics.Canvas (CanvasElement, CanvasImageSource, getCanvasDimensions, getCanvasHeight, getCanvasWidth, setCanvasDimensions)
import Graphics.Canvas (Dimensions) as Canvas
import Seegee.DevicePixelRatio (DevicePixelRatio)
import Seegee.DevicePixelRatio (conversionFactor) as DevicePixelRatio
import Seegee.Geometry.Distance.Units (Pixel, Screen) as Units
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLCanvasElement)
import Web.HTML.HTMLCanvasElement (toHTMLElement) as HTMLCanvasElement

-- | I'm not sure if it is worth to provide global values.
-- | `crips-edges` has really little support.
-- |
-- | image-rendering: auto;
-- | image-rendering: crisp-edges;
-- | image-rendering: pixelated;
-- |
-- | /* Global values */
-- | image-rendering: inherit;
-- | image-rendering: initial;
-- | image-rendering: unset;
data ImageRendering = Auto | Pixelated

serImageRendering ∷ ImageRendering → String
serImageRendering Auto = "auto"
serImageRendering Pixelated = "pixelated"

setImageRendering ∷ ImageRendering → CanvasElement → Effect Unit
setImageRendering imageRendering canvasElement = do
  setStyleProperty "image-rendering" (serImageRendering imageRendering) elem
  where
    elem = HTMLCanvasElement.toHTMLElement (toHTMLCanvasElement canvasElement)

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
  setStyleProperty "width" (serDistancePx dimensions.width) elem
  setStyleProperty "height" (serDistancePx dimensions.height) elem
  where
    elem = HTMLCanvasElement.toHTMLElement (toHTMLCanvasElement canvasElement)
    serDistancePx d = (show $ Distance.toNumber d) <> "px"

setDimensions ∷ Dimensions → CanvasElement → Effect Unit
setDimensions dimensions canvasElement = do
  setPhysicalDimensions dimensions.physical canvasElement
  setLogicalDimensions dimensions.logical canvasElement

-- | XXX: provide more methods
physicalDimensions ∷ CanvasElement → Effect (Geometry.Plane.Dimensions Units.Pixel)
physicalDimensions canvasElement = map Dimensions.unsafe $
  { height: _, width: _} <$> getCanvasHeight canvasElement <*> getCanvasWidth canvasElement

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

-- foreign import clipPath2DImpl ∷ EffectFn3 Context2D Path2D String Unit
-- 
-- -- | "nonzero" is default value used by clip.
-- data FillRule
--   = NonZero
--   | EvenOdd
-- 
-- clipPath ∷ Context2D → Path2D → FillRule → Effect Unit
-- clipPath ctx path = case _ of
--   NonZero → runEffectFn3 clipPath2DImpl ctx path "nonzero"
--   EvenOdd → runEffectFn3 clipPath2DImpl ctx path "evenodd"
-- 
