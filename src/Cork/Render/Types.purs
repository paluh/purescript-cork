module Cork.Render.Types where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Traversable (class Traversable, traverseDefault)
import Geometry.Plane (BoundingBox, Point)
import Seegee.Geometry.Distance.Units (Pixel) as Units

-- | Move hash outside
data DrawCanvasImageSourceF canvasImageSource
  = DrawImage
    (Point Units.Pixel)
    canvasImageSource
  | DrawImageScale
    (BoundingBox Units.Pixel)
    canvasImageSource
derive instance functorDrawCanvasImageSourceF ∷ Functor DrawCanvasImageSourceF
instance foldableDrawCanvasImageSourceF ∷ Foldable DrawCanvasImageSourceF where
  foldMap f (DrawImage _ c) = f c
  foldMap f (DrawImageScale _ c) = f c

  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableDrawCanvasImageSourceF ∷ Traversable DrawCanvasImageSourceF where
  sequence (DrawImage p c) = DrawImage p <$> c
  sequence (DrawImageScale bb c) = DrawImageScale bb <$> c

  traverse = traverseDefault

data DrawImageDataF imageData = PutImageData (Point Units.Pixel) imageData
derive instance functorDrawImageDataF ∷ Functor DrawImageDataF

newtype DrawF imageData canvasImageSource = DrawF (Either (DrawImageDataF imageData) (DrawCanvasImageSourceF canvasImageSource))
instance bifunctorDrawF ∷ Bifunctor DrawF where
  bimap f g (DrawF e) = DrawF (bimap (map f) (map g) e)
