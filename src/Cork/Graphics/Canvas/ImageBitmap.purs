module Cork.Graphics.Canvas.ImageBitmap
  ( module Clip
  , module Draw
  , module Projection
  , module Types
  , drawImage
  , drawImageScale
  )
  where

import Prelude

import Cork.Graphics.Canvas.Context2D (TilesNumber, drawImagePerspective) as Context2D
import Cork.Graphics.Canvas.ImageBitmap.Draw (drawImageFull, drawImagePerspective) as Draw
import Cork.Graphics.Canvas.ImageBitmap.Clip (clip, clipToImageData, clipToMutableImageData) as Clip
import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap, dimensions')
import Cork.Graphics.Canvas.ImageBitmap.Types (dimensions, dimensions', fromAnyImageData, fromHTMLLoadedImageElement, fromSource, height, height', ImageBitmap, selfCreateImageBitmapFromImage, selfCreateImageBitmapFromImageData, selfUnsafeCreateImageBitmap, toCanvasImageSource, toImageData, toMutableImageData, width, width') as Types
import Cork.Graphics.Canvas.ImageBitmap.Projection (perspective, perspectiveToImageData, perspectiveToMutableImageData) as Projection
import Cork.Graphics.Canvas.ImageBitmap.Types (toCanvasImageSource) as ImageBitmap
import Effect (Effect)
import Geometry.Distance (Distance)
import Geometry.Plane (BoundingBox(..), Point(..), point)
import Geometry.Plane.Figures.Polygons.Quadrilateral (Quadrilateral)
import Graphics.Canvas (CanvasImageSource, Context2D)
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

