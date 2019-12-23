module Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale.Types where

data Mode
  = Average
  | Lightness
  | Luminosity

serMode ∷ Mode → String
serMode Average = "average"
serMode Lightness = "lightness"
serMode Luminosity = "luminosity"
