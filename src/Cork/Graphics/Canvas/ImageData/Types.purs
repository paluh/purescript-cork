module Cork.Graphics.Canvas.ImageData.Types where

import Graphics.Canvas (ImageData)
import Cork.Graphics.Canvas.ImageData.Mutable (Mutable) as ImageData.Mutable

data AnyImageData
  = Mutable ImageData.Mutable.Mutable
  | Immutable ImageData
