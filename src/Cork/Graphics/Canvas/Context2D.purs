module Cork.Graphics.Canvas.Context2D where

import Prelude

import Cork.Graphics.Canvas.Path2D (Path2D)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Geometry.Plane (BoundingBox)
import Graphics.Canvas (Context2D)
import Seegee.Geometry.Distance.Units (Pixel) as Units

foreign import clearRect ∷ Context2D → BoundingBox Units.Pixel → Effect Unit

foreign import clipPath2DImpl ∷ EffectFn3 Context2D Path2D String Unit

-- | "nonzero" is default value used by clip.
data FillRule
  = NonZero
  | EvenOdd

clipPath ∷ Context2D → Path2D → FillRule → Effect Unit
clipPath ctx path = case _ of
  NonZero → runEffectFn3 clipPath2DImpl ctx path "nonzero"
  EvenOdd → runEffectFn3 clipPath2DImpl ctx path "evenodd"

foreign import resetTransform ∷ Context2D → Effect Unit

