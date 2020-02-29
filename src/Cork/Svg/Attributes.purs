module Cork.Svg.Attributes where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String (joinWith) as String
import Debug.Trace (traceM)
import Geometry (Angle(..), Distance(..))
import Geometry.Angle (toDegrees)
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Plane (Point(..))
import Geometry.Plane.BoundingBox (BoundingBox(..))
import Math (pi) as Math
import Seegee.Color (Color)
import Seegee.Geometry.Distance.Units (Scene) as Units
import Cork.Svg.Transform (Transform(..))
import Cork.Svg.Types (ViewBox)

-- | TODO: Use serialization functions from `Utils.React.Basic.DOM.Svg`

color ∷ Color → String
color c = "#" <> c.repr

viewBox ∷ ViewBox → String
viewBox (BoundingBox { height: Distance (NonNegative height), width: Distance (NonNegative width), x, y }) =
  String.joinWith " " [ show x, show y, show width, show height]

transform ∷ Transform → String
transform (Rotate { angle, center: Point center }) =
    joinWith " " ["rotate(", show (toDegrees angle), show center.x, show center.y, ")"]
transform (Cons a b) =
  transform a <> " " <> transform b

points ∷ Array (Point Units.Scene) → String
points = joinWith " " <<< map (\(Point p) → show p.x <> " " <> show p.y)
