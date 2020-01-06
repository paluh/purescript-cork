module Cork.Render.Zoom where

import Prelude

import Geometry.Plane (Vector(..))
import Geometry.Plane.Vector (scale) as Vector

type Ratio = Number
newtype Zoom = Zoom { pan ∷ Vector, ratio ∷ Ratio }

instance semigroupZoom ∷ Semigroup Zoom where
  append (Zoom z1@{ pan: Vector p1 }) (Zoom z2@{ pan: Vector p2 }) = Zoom
    let
      pan = z1.pan <> (Vector.scale (1.0 / z1.ratio) z2.pan)
    in
      { pan, ratio: z1.ratio * z2.ratio }

instance monoidZoom ∷ Monoid Zoom where
  mempty = Zoom { ratio: 1.0, pan: mempty }

-- zoom ∷ ∀ u. Zoom → ViewBox u → ViewBox u
-- zoom (Zoom z@{ pan: Vector p, ratio }) vb@(BoundingBox vbr@{ height: Distance (NonNegative height), width: Distance (NonNegative width) }) =
--   let
--     pan = vector (p.x * width) (p.y * height)
--     Point center = translatePoint pan (BoundingBox.center vb)
-- 
--     height' = height / ratio
--     width' = width / ratio
-- 
--     x = center.x - width' / 2.0
--     y = center.y - height' / 2.0
--   in
--     BoundingBox { x, y, height: Distance (NonNegative height'), width: Distance (NonNegative width') }
-- 
