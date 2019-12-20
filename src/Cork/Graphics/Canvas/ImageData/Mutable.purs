module Cork.Graphics.Canvas.ImageData.Mutable
  ( module Types
  -- , scale
  -- , scale'
  )
  where

import Prelude

import Cork.Graphics.Canvas.ImageData.Immutable (fromHTMLLoadedImageElement) as Immutable
import Cork.Graphics.Canvas.ImageData.Mutable.Types (Mutable, unsafeThaw)
import Cork.Graphics.Canvas.ImageData.Mutable.Types (freeze, getImageData, thaw, Mutable, unsafeFreeze, unsafeThaw) as Types
import Cork.Web.HTML.HTMLLoadedImageElement (HTMLLoadedImageElement)
import Effect (Effect)
import Graphics.Canvas (CanvasElement)

fromHTMLLoadedImageElement ∷ CanvasElement → HTMLLoadedImageElement → Effect Mutable
fromHTMLLoadedImageElement canvas = Immutable.fromHTMLLoadedImageElement canvas >=> unsafeThaw
