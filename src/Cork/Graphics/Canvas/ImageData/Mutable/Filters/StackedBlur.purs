module Cork.Graphics.Canvas.ImageData.Mutable.Filters.StackedBlur where

import Effect (Effect)
import Prelude (Unit)
import Cork.Graphics.Canvas.ImageData.Mutable.Types (Mutable)

foreign import filter ∷ Mutable → Int → Effect Unit

