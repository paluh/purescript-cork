module Cork.Svg.Path.BoundingBox where

import Geometry.Plane (BoundingBox)
import Cork.Svg.Path (Repr)

foreign import boundingBox ∷ ∀ u. Repr → BoundingBox u
