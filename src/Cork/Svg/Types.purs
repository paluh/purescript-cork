module Cork.Svg.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Newtype (class Newtype)
import Geometry (kind SpaceUnit)
import Geometry.Plane.BoundingBox (BoundingBox)
import Seegee.Geometry.Distance.Units (Scene) as Units
import Text.Smolder.SVG (Svg)

type Defs e = Svg e

type Body e = Svg e

type ViewBox = BoundingBox Units.Scene

newtype Document e = Document { viewBox ∷ ViewBox, defs ∷ Defs e, body ∷ Body e }

newtype Render = Render String
derive instance newtypeRender ∷ Newtype Render _

instance encodeJson ∷ EncodeJson Render where
  encodeJson (Render s) = encodeJson s

instance decodeJson ∷ DecodeJson Render where
  decodeJson v = Render <$> decodeJson v



