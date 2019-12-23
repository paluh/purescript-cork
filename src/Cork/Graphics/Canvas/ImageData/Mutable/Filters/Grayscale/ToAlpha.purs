module Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale.ToAlpha where

import Prelude

import Cork.Graphics.Canvas.ImageData.Mutable (Mutable)
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale.Types (Mode, serMode)
import Effect (Effect)

foreign import filterImpl ∷ String → Mutable → Effect Unit

filter ∷ Mode → Mutable → Effect Unit
filter = filterImpl <<< serMode

