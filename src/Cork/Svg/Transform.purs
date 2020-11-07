module Cork.Svg.Transform where

import Prelude

import Data.Maybe (Maybe)
import Geometry (Angle)
import Geometry.Numbers (NonNegative) as Numbers
import Geometry.Plane (Point, Translation)
import Geometry.Plane.Transformations.Isometries.Translation (position')
import Seegee.Geometry.Distance.Units (Scene) as Units

data Transform
  = Rotate Angle (Maybe (Point Units.Scene))
  | Translate (Translation Units.Scene)
  | Scale Numbers.NonNegative Numbers.NonNegative
  | SkewX Angle
  | SkewY Angle
  | Transforms Transform Transform

instance semigroupTransform ∷ Semigroup Transform where
  append a b = Transforms a b

translate ∷ Number → Number → Transform
translate x y = Translate (position' x y)
