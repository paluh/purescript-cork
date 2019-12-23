module Cork.Graphics.Canvas.ImageData.Mutable.Scale where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Geometry.Plane.BoundingBox.Dimensions (Dimensions)
import Graphics.Canvas (CanvasElement, ImageData)
import Seegee.Geometry.Distance.Units (Pixel) as Units

-- scale ∷ Mutable → Dimensions Units.Pixel → CanvasElement → Aff Mutable
-- scale img bb canvas = liftEffect <<< unsafeThaw =<< scale' img bb canvas
-- 
-- scale' ∷ Mutable → Dimensions Units.Pixel → CanvasElement → Aff Immutable
-- scale' img bb canvas
--   = (\img' → ImageData.Transforms.Scale.scale img' bb canvas)
--   =<< liftEffect (unsafeFreeze img)
