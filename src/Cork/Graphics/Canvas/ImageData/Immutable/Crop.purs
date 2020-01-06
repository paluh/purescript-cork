module Cork.Graphics.Canvas.ImageData.Immutable.Crop where


-- import Cork.Graphics.Canvas.CanvasElement (new') as CanvasElement
-- import Cork.Graphics.Canvas.ImageBitmap.Crop (cropToMutableImageData)
-- import Cork.Graphics.Canvas.ImageBitmap.Types (fromAnyImageData)
-- import Cork.Graphics.Canvas.ImageData.Mutable.Types (Mutable, dimensions)
-- import Cork.Graphics.Canvas.ImageData.Types (AnyImageData(..))
-- import Cork.Hashable (boundingBox)
-- import Data.Maybe (Maybe(..), maybe)
-- import Effect.Aff (Aff)
-- import Effect.Class (liftEffect)
-- import Geometry.Plane (BoundingBox(..))
-- import Graphics.Canvas (CanvasElement)
-- import Seegee.Geometry.Distance.Units (Pixel) as Units
-- 
-- -- | Maybe we should drop this `Maybe` and require a canvas here
-- crop ∷ Maybe CanvasElement → BoundingBox Units.Pixel → ImageData → Aff ImageData
-- crop possibleCanvas boundingBox orig = do
--   let
--     d = dimensions orig
--   canvas ← liftEffect $ maybe (CanvasElement.new' ) pure  possibleCanvas
--   imageBitmap ← fromAnyImageData canvas (Mutable orig)
--   liftEffect $ cropToMutableImageData (Just canvas) boundingBox imageBitmap
-- 
