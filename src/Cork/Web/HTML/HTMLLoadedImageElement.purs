module Cork.Web.HTML.HTMLLoadedImageElement where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (new) as CanvasElement
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (fromJust)
import Data.String.Utils (startsWith) as Data.String.Utils
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Geometry (Distance)
import Geometry.Distance (ConversionFactor)
import Geometry.Distance.ConversionFactor (unsafe) as ConversionFactor
import Geometry.Plane.BoundingBox (Dimensions)
import Geometry.Plane.BoundingBox.Dimensions (convert) as Dimensions
import Graphics.Canvas (CanvasElement, drawImage, getContext2D)
import Graphics.Canvas (CanvasImageSource) as Canvas
import Partial.Unsafe (unsafePartial)
import Seegee.Geometry.Distance.Units (Pixel, Screen) as Units
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (getAttribute)
import Web.HTML (HTMLElement, HTMLImageElement)
import Web.HTML.HTMLElement (toElement) as HTMLElement
import Web.HTML.HTMLImageElement (setHeight, setWidth, toHTMLElement) as HTMLImageElement

type ImageDataURL = String

data Source
  = URL String
  | DataURL ImageDataURL
derive instance genericSource ∷ Generic Source _
derive instance eqSource ∷ Eq Source
instance hashableSource ∷ Hashable Source where
  hash (DataURL d) = hash d
  hash (URL u) = hash u

instance encodeJsonSource ∷ EncodeJson Source where
  encodeJson = genericEncodeJson

instance decodeJsonSource ∷ DecodeJson Source where
  decodeJson = genericDecodeJson

newtype HTMLLoadedImageElement = HTMLLoadedImageElement HTMLImageElement

foreign import setSrcImpl ∷ String → HTMLLoadedImageElement → EffectFnAff Unit

-- | The same can be done for ImageElement
setSrc ∷ Source → HTMLLoadedImageElement → Aff Unit
setSrc (URL src) img = fromEffectFnAff (setSrcImpl src img)
setSrc (DataURL src) img = fromEffectFnAff (setSrcImpl src img)

setDataURLSrc ∷ ImageDataURL → HTMLLoadedImageElement → Aff Unit
setDataURLSrc src img = fromEffectFnAff (setSrcImpl src img)

getSrc ∷ HTMLLoadedImageElement → Effect Source
getSrc img = do
  rawSrc ← unsafePartial (fromJust <$> getAttribute "src" (HTMLElement.toElement (toHTMLElement img)))
  pure $ if (Data.String.Utils.startsWith "data" rawSrc)
    then DataURL rawSrc
    else URL rawSrc

foreign import newImpl ∷ String → EffectFnAff HTMLLoadedImageElement

new ∷ Source → Aff HTMLLoadedImageElement
new (URL src) = fromEffectFnAff (newImpl src)
new (DataURL src) = fromEffectFnAff (newImpl src)

-- | Ensure HTMLImageElement is loaded
-- load ∷ HTMLImageElement → Aff HTMLLoadedImageElement

clone ∷ HTMLLoadedImageElement → Aff HTMLLoadedImageElement
clone img = do
  src ← liftEffect (getSrc img)
  new src

-- | We don't really want to start the path into "discrate geometry"
-- | Browsers are able to deal with floats in case of this props:
-- | https://stackoverflow.com/questions/15300163/how-do-browsers-deal-with-non-integer-values-for-height-and-width
setHeight ∷ Distance Units.Screen → HTMLLoadedImageElement → Effect Unit
setHeight height img = HTMLImageElement.setHeight (unsafeCoerce height) (toHTMLImageElement img)

setWidth ∷ Distance Units.Screen → HTMLLoadedImageElement → Effect Unit
setWidth width img = HTMLImageElement.setWidth (unsafeCoerce width) (toHTMLImageElement img)

url ∷ Source → String
url (URL u) = u
url (DataURL u) = u

foreign import toDataURLImpl ∷ Dimensions Units.Pixel → HTMLLoadedImageElement → Effect String

-- | We need this raw pixel data URLs (instead of just data:image/svg+xml)
-- | to use them in different contexts like:
-- |  * gltf textures
-- |  * scene sprites
toDataURL ∷ Dimensions Units.Pixel → HTMLLoadedImageElement → Effect Source
toDataURL dimensions img = DataURL <$> toDataURLImpl dimensions img

foreign import naturalDimensions' ∷ HTMLLoadedImageElement → { height ∷ Int, width ∷ Int }

naturalDimensions ∷ HTMLLoadedImageElement → Dimensions Units.Pixel
naturalDimensions = naturalDimensions' >>> unsafeFromInt
  where
    unsafeFromInt ∷ { height ∷ Int, width ∷ Int } → Dimensions Units.Pixel
    unsafeFromInt = unsafeCoerce

toHTMLElement ∷ HTMLLoadedImageElement → HTMLElement
toHTMLElement (HTMLLoadedImageElement i) = HTMLImageElement.toHTMLElement i

toHTMLImageElement ∷ HTMLLoadedImageElement → HTMLImageElement
toHTMLImageElement (HTMLLoadedImageElement i) = i

unsafeFromHTMLImageElement ∷ HTMLImageElement → HTMLLoadedImageElement
unsafeFromHTMLImageElement i = HTMLLoadedImageElement i

toCanvasImageSource ∷ HTMLLoadedImageElement → Canvas.CanvasImageSource
toCanvasImageSource = unsafeCoerce

toCanvasElement ∷ HTMLLoadedImageElement → Effect CanvasElement
toCanvasElement img = do
  let
    physical = naturalDimensions img
    pxToScreen ∷ ConversionFactor Units.Pixel Units.Screen
    pxToScreen = ConversionFactor.unsafe 1.0
    logical = Dimensions.convert pxToScreen physical
  canvas ← CanvasElement.new { logical, physical }
  context ← getContext2D canvas
  drawImage
    context
    (toCanvasImageSource img)
    0.0
    0.0
  pure canvas

