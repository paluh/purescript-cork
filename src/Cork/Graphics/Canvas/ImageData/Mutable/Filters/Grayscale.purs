module Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale
  ( filter
  , module Types
  )
  where

import Prelude

import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale.Types (Mode, serMode)
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale.Types (Mode(..), serMode) as Types
import Cork.Graphics.Canvas.ImageData.Mutable.Types (Mutable)
import Effect (Effect)

foreign import filterImpl ∷ String → Mutable → Effect Unit

filter ∷ Mode → Mutable → Effect Unit
filter = filterImpl <<< serMode

