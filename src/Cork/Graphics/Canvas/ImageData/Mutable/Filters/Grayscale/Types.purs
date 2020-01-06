module Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale.Types where

import Prelude

import Data.Hashable (class Hashable, hash)

data Mode
  = Average
  | Lightness
  | Luminosity

derive instance eqMode ∷ Eq Mode
derive instance ordMode ∷ Ord Mode
instance hashable ∷ Hashable Mode where
  hash = hash <<< serMode

serMode ∷ Mode → String
serMode Average = "average"
serMode Lightness = "lightness"
serMode Luminosity = "luminosity"
