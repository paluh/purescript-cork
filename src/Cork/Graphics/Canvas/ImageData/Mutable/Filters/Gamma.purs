module Cork.Graphics.Canvas.ImageData.Mutable.Filters.Gamma where

import Effect (Effect)
import Prelude (Unit)
import Cork.Graphics.Canvas.ImageData.Mutable.Types (Mutable)

-- gamma values should be 0.01 to 2.2.
type Gamma = { r ∷ Number, g ∷ Number, b ∷ Number }
foreign import filter ∷ Gamma → Mutable → Effect Unit

