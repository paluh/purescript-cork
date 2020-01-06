module Cork.Graphics.Canvas.ImageData.Immutable
  ( fromHTMLLoadedImageElement
  , module Types
  , toHTMLLoadedImageElement
  )
  where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (new') as CanvasElement
import Cork.Graphics.Canvas.CanvasElement (setMinCanvasPhysicalDimensions, setPhysicalDimensions)
import Cork.Graphics.Canvas.ImageData.Immutable.Types (dimensions, height, Immutable, width) as Types
import Cork.Web.HTML.HTMLLoadedImageElement (HTMLLoadedImageElement, Source(..))
import Cork.Web.HTML.HTMLLoadedImageElement (naturalDimensions, new, toCanvasImageSource) as HTMLLoadedImageElement
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Geometry.Distance (toNumber) as Distance
import Graphics.Canvas (CanvasElement, ImageData, canvasToDataURL, drawImage, getContext2D, getImageData, putImageData)

fromHTMLLoadedImageElement ∷ Maybe CanvasElement → HTMLLoadedImageElement → Effect ImageData
fromHTMLLoadedImageElement possibleCanvas img = case possibleCanvas of
  Just canvas → go canvas
  Nothing → do
    let
      physical = HTMLLoadedImageElement.naturalDimensions img
    canvas ← CanvasElement.new' physical
    go canvas
  where
    go canvas = do
      let
        d = HTMLLoadedImageElement.naturalDimensions img
      setMinCanvasPhysicalDimensions canvas d
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
        (Distance.toNumber d.width)
        (Distance.toNumber d.height)

toHTMLLoadedImageElement ∷ Maybe CanvasElement → ImageData → Aff HTMLLoadedImageElement
toHTMLLoadedImageElement possibleCanvas imageData = case possibleCanvas of
  Just canvas → go canvas
  Nothing → do
    canvas ← liftEffect $ CanvasElement.new' physical
    go canvas
  where
    physical = Types.dimensions imageData
    go canvas = do
      url ← liftEffect $ do
        setPhysicalDimensions physical canvas
        ctx ← getContext2D canvas
        putImageData ctx imageData 0.0 0.0
        canvasToDataURL canvas
      HTMLLoadedImageElement.new (DataURL url)


