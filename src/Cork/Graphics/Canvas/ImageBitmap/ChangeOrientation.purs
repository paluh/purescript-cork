module Cork.Graphics.Canvas.ImageBitmap.ChangeOrientation where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (new') as CanvasElement
import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap, dimensions, fromAnyImageData)
import Cork.Graphics.Canvas.ImageData.Types (AnyImageData(..))
import Cork.Graphics.Canvas.ImageData.Mutable.Types (Mutable) as ImageData
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Graphics.Canvas (CanvasElement)

data OrientationChange = Rotate90 | Rotate180 | Rotate270

foreign import changeOrientationToMutableImageDataImpl ∷ CanvasElement → Int → ImageBitmap → Effect ImageData.Mutable

changeOrientationToMutableImageData ∷ Maybe CanvasElement → OrientationChange → ImageBitmap → Effect ImageData.Mutable
changeOrientationToMutableImageData possibleCanvas change orig = do
  -- | XXX: Use square canvas with max dimensions
  canvas ← liftEffect $ maybe (CanvasElement.new' (dimensions orig)) pure possibleCanvas
  changeOrientationToMutableImageDataImpl canvas change' orig
  where
    change' = case change of
      Rotate90 → 1
      Rotate180 → 2
      Rotate270 → 3

changeOrientation ∷ Maybe CanvasElement → OrientationChange → ImageBitmap → Aff ImageBitmap
changeOrientation possibleCanvas change orig = do
  canvas ← liftEffect $ maybe (CanvasElement.new' (dimensions orig)) pure possibleCanvas
  mutable ← liftEffect $ changeOrientationToMutableImageData (Just canvas) change orig
  fromAnyImageData canvas (Mutable mutable)
