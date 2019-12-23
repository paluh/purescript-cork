module Cork.Graphics.Canvas.ImageData.Mutable
  ( module Types
  , module ChangeOrientation
  , fromHTMLLoadedImageElement
  , toHTMLLoadedImageElement
  -- , scale
  -- , scale'
  )
  where

import Prelude

import Cork.Graphics.Canvas.ImageData.Immutable (fromHTMLLoadedImageElement, toHTMLLoadedImageElement) as Immutable
import Cork.Graphics.Canvas.ImageData.Mutable.ChangeOrientation (changeOrientation) as ChangeOrientation
import Cork.Graphics.Canvas.ImageData.Mutable.Types (Mutable, unsafeFreeze, unsafeThaw)
import Cork.Graphics.Canvas.ImageData.Mutable.Types (dimensions, freeze, getImageData, thaw, Mutable, unsafeFreeze, unsafeThaw) as Types
import Cork.Web.HTML.HTMLLoadedImageElement (HTMLLoadedImageElement)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Graphics.Canvas (CanvasElement)

fromHTMLLoadedImageElement ∷ Maybe CanvasElement → HTMLLoadedImageElement → Effect Mutable
fromHTMLLoadedImageElement possibleCanvas = Immutable.fromHTMLLoadedImageElement possibleCanvas >=> unsafeThaw

toHTMLLoadedImageElement ∷ Maybe CanvasElement → Mutable → Aff HTMLLoadedImageElement
toHTMLLoadedImageElement possibleCanvas = unsafeFreeze >>> liftEffect >=> Immutable.toHTMLLoadedImageElement possibleCanvas
