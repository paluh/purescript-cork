module Cork.Utils.Graphics.ImageData.Immutable.Scale where

import Prelude

-- import Cork.Graphics.Canvas.CanvasElement (minCanvasPhysicalDimensions)
import Data.Int (toNumber) as Int
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Geometry.Distance (toNumber) as Distance
import Geometry.Plane.BoundingBox (Dimensions)
import Graphics.Canvas (CanvasElement, ImageData, drawImageFull, drawImageScale, getContext2D, getImageData, imageDataHeight, imageDataWidth)
import Seegee.Geometry.Distance.Units (Pixel) as Units
import Unsafe.Coerce (unsafeCoerce)

-- scale ∷ ImageData → Dimensions Units.Pixel → CanvasElement → Aff ImageData
-- scale imageData dimensions canvas = do
--   let
--     width' = Distance.toNumber dimensions.width
--     height' = Distance.toNumber dimensions.height
--   img ← ImageData.toCanvasImageSource imageData
--   liftEffect $ do
--     minCanvasPhysicalDimensions canvas (unsafeCoerce dimensions)
--     ctx ← getContext2D canvas
--     drawImageFull
--       ctx
--       img
--       0.0
--       0.0
--       (Int.toNumber $ imageDataWidth imageData)
--       (Int.toNumber $ imageDataHeight imageData)
--       0.0
--       0.0
--       200.0
--       200.0
--       -- width'
--       -- height'
--     getImageData ctx 0.0 0.0 300.0 300.0 -- width' height'
-- 
-- 
