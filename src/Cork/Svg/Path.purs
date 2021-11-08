module Cork.Svg.Path where

import Prelude

import Data.Array (fromFoldable) as Array
import Data.List (List)
import Data.String (joinWith) as String
import Geometry (Angle, Distance)
import Geometry.Angle (toDegrees) as Angle
import Geometry.Distance (toNumber) as Distance
import Geometry.Line.Point (Point(..)) as Line
import Geometry.Plane (Point(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (uncons) as Array.NonEmpty
import Data.Foldable (foldr)
import Data.List (List(..)) as List

-- | Path definition which can be used by both
-- | Svg and Canvas.Path2d.

type Coordinate u = Line.Point u
newtype BezierControl u = BezierControl (Point u)
derive instance eqBezierControl ∷ Eq (BezierControl u)

data Arc = Small | Large
derive instance eqArc ∷ Eq Arc

data Sweep = Clockwise | Counterclockwise
derive instance eqSweep ∷ Eq Sweep

type Radius u =
  { rx ∷ Distance u
  , ry ∷ Distance u
  }
type Rotation = Angle
data Command u
  = ArcAbs (Radius u) Rotation Arc Sweep (Point u)
  | ArcRel (Radius u) Rotation Arc Sweep (Point u)
  | BezierCurveAbs (BezierControl u) (BezierControl u) (Point u)
  | BezierCurveRel (BezierControl u) (BezierControl u) (Point u)
  | BezierQuadraticAbs (BezierControl u) (Point u)
  | BezierQuadraticRel (BezierControl u) (Point u)
  | BezierQuadraticSegmentAbs (Point u)
  | BezierQuadraticSegmentRel (Point u)
  | BezierSegmentAbs (BezierControl u) (Point u)
  | BezierSegmentRel (BezierControl u) (Point u)
  | Close
  | HorizontalAbs (Coordinate u)
  | HorizontalRel (Coordinate u)
  | LineAbs (Point u)
  | LineRel (Point u)
  | MoveAbs (Point u)
  | MoveRel (Point u)
  | VerticalAbs (Coordinate u)
  | VerticalRel (Coordinate u)
derive instance eqCommand ∷ Eq (Command u)

type Path u = List (Command u)

printCommand ∷ ∀ u. Command u → String
printCommand = case _ of
  (ArcAbs r rotation arc sweep p) ->
    "A " <> printArc r rotation arc sweep p
  (ArcRel r rotation arc sweep p) ->
    "a " <> printArc r rotation arc sweep p
  (BezierCurveAbs c1 c2 p) ->
    "C " <> printBezierCurve c1 c2 p
  (BezierCurveRel c1 c2 p) ->
    "c " <> printBezierCurve c1 c2 p
  (BezierQuadraticAbs c p) ->
    "Q " <> printBezierQuadratic c p
  (BezierQuadraticRel c p) ->
    "q " <> printBezierQuadratic c p
  (BezierQuadraticSegmentAbs p) ->
    "T " <> printPoint p
  (BezierQuadraticSegmentRel p) ->
    "t " <> printPoint p
  (BezierSegmentAbs c p) ->
    "S " <> printBezierSegment c p
  (BezierSegmentRel c p) ->
    "s " <> printBezierSegment c p
  Close -> "z"
  (HorizontalAbs c) ->
    "H " <> printCoordinate c
  (HorizontalRel c) ->
    "h " <> printCoordinate c
  (LineAbs p) ->
    "L " <> printPoint p
  (LineRel p) ->
    "l " <> printPoint p
  (MoveAbs p) ->
    "M " <> printPoint p
  (MoveRel p) ->
    "m " <> printPoint p
  (VerticalAbs c) ->
    "V " <> printCoordinate c
  (VerticalRel c) ->
    "v " <> printCoordinate c
  where
    printPoint (Point { x, y }) = show x <> " " <> show y

    printArc { rx, ry } rotation arc sweep end = String.joinWith " "
      [ show (Distance.toNumber rx)
      , show (Distance.toNumber ry)
      , show (Angle.toDegrees rotation)
      , case arc of
          Small → "0"
          Large → "1"
      , case sweep of
          Clockwise → "1"
          Counterclockwise → "0"
      , printPoint end
      ]

    printCoordinate (Line.Point p) = show p

    printBezierControl (BezierControl p) = printPoint p

    printBezierCurve c1 c2 end = String.joinWith " "
      [ printBezierControl c1
      , printBezierControl c2
      , printPoint end
      ]

    printBezierQuadratic c end = String.joinWith " "
      [ printBezierControl c
      , printPoint end
      ]

    printBezierSegment c end = String.joinWith " "
      [ printBezierControl c
      , printPoint end
      ]

newtype Repr = Repr String

print ∷ ∀ u. Path u → Repr
print =
  Repr <<< String.joinWith " " <<< map printCommand <<< Array.fromFoldable

unRepr ∷ Repr → String
unRepr (Repr s) = s


fromPoints :: forall u. NonEmptyArray (Point u) -> Path u
fromPoints points =
  let
    { head, tail } = Array.NonEmpty.uncons points
    step pnt pth = List.Cons (LineAbs pnt) pth
  in
    List.Cons (MoveAbs head) $ foldr step (List.Cons Close List.Nil) points
