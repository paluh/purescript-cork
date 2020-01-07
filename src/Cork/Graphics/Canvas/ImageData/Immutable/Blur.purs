module Cork.Graphics.Canvas.ImageData.Immutable.Filters.Blur where

import Prelude

import Data.Hashable (class Hashable)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, ImageData)
import Math ((%))

-- |Number between 0.0 - 1.0
newtype Blur = Blur Number
derive newtype instance eqBlur ∷ Eq Blur
derive newtype instance ordBlur ∷ Ord Blur
derive newtype instance hashableBlur ∷ Hashable Blur

blur ∷ Number → Blur
blur n = Blur (n % 1.0)

foreign import filter ∷ CanvasElement → CanvasElement → Blur → ImageData → Effect ImageData
