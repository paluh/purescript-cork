module Cork.Render.Types where

-- | We should rename this to Drawing or something...

import Prelude

import Data.Bifoldable (class Bifoldable, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either, either)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Newtype (class Newtype)
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
instance foldableDrawImageDataF ∷ Foldable DrawImageDataF where
  foldMap f (PutImageData _ c) = f c

  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableDrawImageDataF ∷ Traversable DrawImageDataF where
  sequence (PutImageData p c) = PutImageData p <$> c

  traverse = traverseDefault

newtype DrawF imageData canvasImageSource = DrawF (Either (DrawImageDataF imageData) (DrawCanvasImageSourceF canvasImageSource))
derive instance newtypeDrawF ∷ Newtype (DrawF imageData canvasImageSource) _
instance bifoldableDrawF ∷ Bifoldable DrawF where
  bifoldMap f g (DrawF d) = either (foldMap f) (foldMap g) d
  bifoldr f g = bifoldrDefault f g
  bifoldl f g = bifoldlDefault f g

instance bifunctorDrawF ∷ Bifunctor DrawF where
  bimap f g (DrawF e) = DrawF (bimap (map f) (map g) e)


-- drawing ∷ (DrawCanvasImageSourceF canvasImageSource → a) → (
