module Cork.Graphics.Canvas.ImageBitmap
  ( module Clip
  , module Types
  , drawImage
  , drawImagePerspective
  , drawImageScale
  , TilesNumber(..)
  )
  where

import Prelude

import Cork.Graphics.Canvas.ImageBitmap.Clip (clip, clipToImageData, clipToMutableImageData) as Clip
import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap, dimensions')
import Cork.Graphics.Canvas.ImageBitmap.Types (dimensions, dimensions', fromAnyImageData, fromHTMLLoadedImageElement, fromSource, height, height', ImageBitmap, selfCreateImageBitmapFromImage, selfCreateImageBitmapFromImageData, selfUnsafeCreateImageBitmap, toCanvasImageSource, toImageData, toMutableImageData, width, width') as Types
import Cork.Graphics.Canvas.ImageBitmap.Types (toCanvasImageSource) as ImageBitmap
import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import Geometry.Distance (Distance)
import Geometry.Plane (BoundingBox(..), Point(..), point)
import Geometry.Plane.Figures.Polygons.Quadrilateral (Quadrilateral)
import Graphics.Canvas (CanvasImageSource, Context2D, PatternRepeat(..), createPattern, setPatternFillStyle)
import Graphics.Canvas (drawImage, drawImageScale) as Canvas
import Seegee.Geometry.Distance.Units (Pixel) as Units
import Unsafe.Coerce (unsafeCoerce)


-- | Some operations require ImageBitmap
-- |
-- | The current path, transformation matrix, shadow attributes, global alpha, the clipping region, and global composition operator
-- | must not affect the getImageData() and putImageData() methods.
-- | https://html.spec.whatwg.org/multipage/canvas.html#dom-context-2d-putimagedata


drawImage ∷ Context2D → ImageBitmap → Point Units.Pixel → Effect Unit
drawImage ctx imageBitmap (Point { x, y }) =
  Canvas.drawImage ctx (ImageBitmap.toCanvasImageSource imageBitmap) x y

drawImageScale ∷ Context2D → ImageBitmap → BoundingBox Units.Pixel → Effect Unit
drawImageScale ctx imageBitmap (BoundingBox { height, width, x, y }) =
  coerceDrawScale Canvas.drawImageScale ctx imageBitmap x y width height
  where
    coerceDrawScale
      ∷ (Context2D → CanvasImageSource → Number → Number → Number → Number → Effect Unit)
      → (Context2D → ImageBitmap → Number → Number → Distance Units.Pixel → Distance Units.Pixel → Effect Unit)
    coerceDrawScale = unsafeCoerce

foreign import fillQuadTexImpl ∷ EffectFn4 Context2D (Quadrilateral Units.Pixel) (Quadrilateral Units.Pixel) TilesNumber Unit

newtype TilesNumber = TilesNumber Int

drawImagePerspective
  ∷ Context2D
  → Quadrilateral Units.Pixel
  → ImageBitmap
  → TilesNumber
  → Effect Unit
drawImagePerspective context dest img tilesNumber = do
  pattern ← createPattern context (ImageBitmap.toCanvasImageSource img) NoRepeat
  -- | XXX: bracket pattern fill style
  setPatternFillStyle context pattern

  let
    { height: h, width: w } = dimensions' img

  runEffectFn4 fillQuadTexImpl
    context
    { first: point 0.0 0.0
    , second: point w 0.0
    , third: point w h
    , fourth: point 0.0 h
    }
    dest
    tilesNumber

