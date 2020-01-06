module Cork.Graphics.Canvas.Path2D where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Cork.Svg.Path (Repr) as Svg.Path

foreign import data Path2D ∷ Type

foreign import new ∷ Svg.Path.Repr → Effect Path2D

