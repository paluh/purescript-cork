module Cork.Hashable where

import Prelude

import Cork.Render.Types (DrawingStyle)
import Cork.Svg.Path (Path) as Svg
import Cork.Svg.Path (print, unRepr) as Svg.Path
import Data.Hashable (hash)
import Data.Tuple.Nested ((/\))
import Geometry.Distance (toNumber) as Distance
import Geometry.Plane (BoundingBox(..), Point(..))
import Geometry.Plane.Figures.Polygons.Quadrilateral (Quadrilateral)
import Geometry.Plane.Point (Point)
import Seegee.Geometry.Distance.Units (Pixel) as Units

type Hash = Int

point ∷ Point Units.Pixel → Hash
point (Point p) = hash p

quadrilateral ∷ Quadrilateral Units.Pixel → Hash
quadrilateral { first, second, third, fourth } =
  hash (point first /\ point second /\ point third /\ point fourth)

boundingBox ∷ BoundingBox Units.Pixel → Hash
boundingBox (BoundingBox { height, width, x, y }) =
  hash (Distance.toNumber height /\ Distance.toNumber width /\ x /\ y)

path ∷ Svg.Path Units.Pixel → Hash
path = hash <<< Svg.Path.unRepr <<< Svg.Path.print

drawingStyle ∷ DrawingStyle → Hash
drawingStyle { alpha, compositeOperation } = hash (alpha /\ show compositeOperation)
