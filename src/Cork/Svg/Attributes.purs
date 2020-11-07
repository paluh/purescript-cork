module Cork.Svg.Attributes where

import Prelude

import Cork.Svg.Transform (Transform(..))
import Cork.Svg.Types (ViewBox)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String (joinWith) as String
import Geometry (Distance(..))
import Geometry.Angle (toDegrees)
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Plane (Point(..), Vector(..))
import Geometry.Plane.BoundingBox (BoundingBox(..))
import Geometry.Plane.Transformations.Isometries.Translation (Translation(..))
import Seegee.Color (Color)
import Seegee.Geometry.Distance.Units (Scene) as Units

-- | TODO: Use serialization functions from `Utils.React.Basic.DOM.Svg`

color ∷ Color → String
color c = "#" <> c.repr

viewBox ∷ ViewBox → String
viewBox (BoundingBox { height: Distance (NonNegative height), width: Distance (NonNegative width), x, y }) =
  String.joinWith " " [ show x, show y, show width, show height]

points ∷ Array (Point Units.Scene) → String
points = joinWith " " <<< map (\(Point p) → show p.x <> " " <> show p.y)

transform ∷ Transform → String
transform = case _ of
  Rotate angle (Just (Point center)) →
    "rotate(" <> show (toDegrees angle) <> " " <> show center.x <> " " <> show center.y <> ")"
  Rotate angle Nothing →
    "rotate(" <> show (toDegrees angle) <> ")"
  Translate (Translation (Vector v)) →
    "translate(" <> show v.x <> " " <> show v.y <> ")"
  Scale x y →
    "scale(" <> show x <> " " <> show y <> ")"
  SkewX angle →
    "skewX( " <> show (toDegrees angle) <> ")"
  SkewY angle →
    "skewY( " <> show (toDegrees angle) <> ")"
  Transforms t1 t2 →
    transform t1 <> " " <> transform t2

