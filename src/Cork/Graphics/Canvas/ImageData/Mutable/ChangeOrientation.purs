module Cork.Graphics.Canvas.ImageData.Mutable.ChangeOrientation
  ( changeOrientation
  , module ChangeOrientation
  )
  where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (new') as CanvasElement
import Cork.Graphics.Canvas.ImageBitmap.ChangeOrientation (OrientationChange, changeOrientationToMutableImageData)
import Cork.Graphics.Canvas.ImageBitmap.ChangeOrientation (OrientationChange(..)) as ChangeOrientation
import Cork.Graphics.Canvas.ImageBitmap.Types (fromAnyImageData)
import Cork.Graphics.Canvas.ImageData.Mutable.Types (Mutable, dimensions)
import Cork.Graphics.Canvas.ImageData.Types (AnyImageData(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Graphics.Canvas (CanvasElement)

changeOrientation ∷ Maybe CanvasElement → OrientationChange → Mutable → Aff Mutable
changeOrientation possibleCanvas orientation orig = do
  canvas ← liftEffect $ maybe (CanvasElement.new' (dimensions orig)) pure possibleCanvas
  imageBitmap ← fromAnyImageData canvas (Mutable orig)
  liftEffect $ changeOrientationToMutableImageData (Just canvas) orientation imageBitmap

