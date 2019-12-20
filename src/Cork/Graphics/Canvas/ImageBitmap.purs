module Cork.Graphics.Canvas.ImageBitmap where

import Prelude

import Control.Promise (Promise)
import Control.Promise (toAff) as Promise
import Cork.Graphics.Canvas.CanvasElement (setPhysicalDimensions)
import Cork.Graphics.Canvas.ImageData (AnyImageData(..))
import Cork.Graphics.Canvas.ImageData (Mutable) as ImageData
import Cork.Graphics.Canvas.ImageData.Immutable (height, width) as ImageData.Immutable
import Cork.Graphics.Canvas.ImageData.Mutable (getImageData, unsafeFreeze) as ImageData.Mutable
import Cork.Web.HTML.HTMLLoadedImageElement (HTMLLoadedImageElement, Source(..))
import Cork.Web.HTML.HTMLLoadedImageElement (Source, clone, naturalDimensions, new, setHeight, setWidth) as HTMLLoadedImageElement
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Geometry.Distance (ConversionFactor, Distance)
import Geometry.Distance (convert, toNumber, unsafeFromInt) as Distance
import Geometry.Distance.ConversionFactor (unsafe) as ConversionFactor
import Graphics.Canvas (CanvasElement, CanvasImageSource, ImageData, canvasToDataURL, getContext2D, putImageData)
import Graphics.Canvas (drawImage) as Canvas
import Seegee.Geometry.Distance.Units (Pixel, Screen) as Units
import Unsafe.Coerce (unsafeCoerce)

-- | We are assuming here that all bitmaps (even these "polyfilled" and represented as image)
-- | are immutable and have height and width.
foreign import data ImageBitmap ∷ Type

-- | Provide an access to `window.createImageBitmap` (or `self.createImageBitmap`) if exists.
-- |
-- | XXX: Implement full pollyfill here as the full spec would allow us to quickly resize images too...
-- | Also the whole process of downloading and decoding an image can be pushed
-- | to webworks with this blob trick.
foreign import selfUnsafeCreateImageBitmapImpl ∷ ∀ img. Effect (Nullable (img → Promise ImageBitmap))

selfUnsafeCreateImageBitmap ∷ ∀ img. Effect (Maybe (img → Aff ImageBitmap))
selfUnsafeCreateImageBitmap = selfUnsafeCreateImageBitmapImpl >>= toMaybe >>> case _ of
  Just createImageBitmap → pure (Just $ createImageBitmap >>> Promise.toAff)
  Nothing → pure Nothing

selfCreateImageBitmapFromImage ∷ Effect (Maybe (HTMLLoadedImageElement → Aff ImageBitmap))
selfCreateImageBitmapFromImage = selfUnsafeCreateImageBitmap

selfCreateImageBitmapFromImageData ∷ Effect (Maybe (ImageData → Aff ImageBitmap))
selfCreateImageBitmapFromImageData = selfUnsafeCreateImageBitmap

height ∷ ImageBitmap → Distance Units.Pixel
height = Distance.unsafeFromInt <<< height'
foreign import height' ∷ ImageBitmap → Int

width ∷ ImageBitmap → Distance Units.Pixel
width = Distance.unsafeFromInt <<< width'
foreign import width' ∷ ImageBitmap → Int

-- | Just a shortcut
fromSource ∷ HTMLLoadedImageElement.Source → Aff ImageBitmap
fromSource = HTMLLoadedImageElement.new >=> fromHTMLLoadedImageElement

fromHTMLLoadedImageElement ∷ HTMLLoadedImageElement → Aff ImageBitmap
fromHTMLLoadedImageElement img = liftEffect selfCreateImageBitmapFromImage >>= case _ of
  Just createImageBitmap → createImageBitmap img
  Nothing → liftEffect $ do
    img' ← HTMLLoadedImageElement.clone img
    let
      { height, width } = HTMLLoadedImageElement.naturalDimensions img'
      cf ∷ ConversionFactor Units.Pixel Units.Screen
      cf = ConversionFactor.unsafe 1.0
    HTMLLoadedImageElement.setHeight (Distance.convert cf height) img'
    HTMLLoadedImageElement.setWidth (Distance.convert cf width) img'
    pure $ unsafeFromHTMLLoadedImageElement img'

-- | Our ImageBitmap "polyfill"
unsafeFromHTMLLoadedImageElement ∷ HTMLLoadedImageElement → ImageBitmap
unsafeFromHTMLLoadedImageElement = unsafeCoerce

fromAnyImageData ∷ CanvasElement → AnyImageData → Aff ImageBitmap
fromAnyImageData workspace = toImageBitmap <=< case _ of
  Mutable mutable → liftEffect $ ImageData.Mutable.unsafeFreeze mutable
  Immutable imageData → pure imageData
  where
    toImageBitmap imageData = liftEffect selfCreateImageBitmapFromImageData >>= case _ of
      Just createImageBitmap → createImageBitmap imageData
      Nothing → do
        dataURL ← liftEffect do
          let
            w = ImageData.Immutable.width imageData
            h = ImageData.Immutable.height imageData
          setPhysicalDimensions { height: h, width: w } workspace
          ctx ← getContext2D workspace
          putImageData
            ctx
            imageData
            0.0
            0.0
          canvasToDataURL workspace
        img ← HTMLLoadedImageElement.new (DataURL dataURL)
        pure $ unsafeFromHTMLLoadedImageElement img
        -- getImageData ctx 0.0 0.0 (Distance.toNumber width) (Distance.toNumber height)

toMutableImageData ∷ CanvasElement → ImageBitmap → Effect ImageData.Mutable
toMutableImageData workspace imageBitmap = do
  let
    w = width imageBitmap
    h = height imageBitmap
  setPhysicalDimensions { height: h, width: w } workspace
  ctx ← getContext2D workspace
  Canvas.drawImage
    ctx
    (toCanvasImageSource imageBitmap)
    0.0
    0.0
  ImageData.Mutable.getImageData ctx 0.0 0.0 (Distance.toNumber w) (Distance.toNumber h)

toImageData ∷ CanvasElement → ImageBitmap → Effect ImageData
toImageData canvas = toMutableImageData canvas >=> ImageData.Mutable.unsafeFreeze

toCanvasImageSource ∷ ImageBitmap → CanvasImageSource
toCanvasImageSource = unsafeCoerce

