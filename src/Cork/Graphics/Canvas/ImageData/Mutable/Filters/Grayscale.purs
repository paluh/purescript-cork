module Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale where

import Effect (Effect)
import Prelude (Unit)
import Cork.Graphics.Canvas.ImageData.Mutable.Types (Mutable)

foreign import filter ∷ Mutable → Effect Unit

