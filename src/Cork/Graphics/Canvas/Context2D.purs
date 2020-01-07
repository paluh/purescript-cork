module Cork.Graphics.Canvas.Context2D where

import Prelude

import Cork.Graphics.Canvas.Path2D (Path2D)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, EffectFn4, runEffectFn3, runEffectFn4)
import Geometry.Plane (BoundingBox)
import Geometry.Plane.Figures.Polygons.Quadrilateral (Quadrilateral)
import Graphics.Canvas (CanvasImageSource, Context2D, PatternRepeat(..), createPattern, setPatternFillStyle)
import Seegee.Geometry.Distance.Units (Pixel) as Units

-- | Provide "primitive" drawing extensions here (based on canvas image source etc).
-- | We don't want to depend on any module from `Cork.Graphics.Canvas` here.

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


foreign import fillQuadTexImpl ∷ EffectFn4 Context2D (Quadrilateral Units.Pixel) (Quadrilateral Units.Pixel) TilesNumber Unit

newtype TilesNumber = TilesNumber Int

drawImagePerspective
  ∷ Context2D
  → CanvasImageSource
  → Quadrilateral Units.Pixel
  → Quadrilateral Units.Pixel
  → TilesNumber
  → Effect Unit
drawImagePerspective context img src dest tilesNumber = do
  pattern ← createPattern context img NoRepeat
  setPatternFillStyle context pattern
  runEffectFn4 fillQuadTexImpl
    context
    src
    dest
    tilesNumber

