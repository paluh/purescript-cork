module Cork.Svg.Transform where

import Prelude

import Geometry (Angle)
import Geometry.Plane (Point)
import Seegee.Geometry.Distance.Units (Scene) as Units

data Transform
  = Rotate { angle ∷ Angle, center ∷ Point Units.Scene }
  -- | Translate (Translation Units.Scene)
  -- | Scale { x ∷ Number, y ∷ Number }
  -- | SkewX Number
  -- | SkewY Number
  | Cons Transform Transform
  -- | Matrix 

instance semigroupTransform ∷ Semigroup Transform where
  append a b = Cons a b

