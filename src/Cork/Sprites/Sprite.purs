module Cork.Sprites.Sprite
  -- ( module Exports
  -- , clip
  -- , crop
  -- , hash
  -- , grayscale
  -- , grayscaleToAlpha
  -- , stackedBlur
  -- , Sprite
  -- , SpriteF
  -- )
  where

import Prelude

import Cork.Graphics.Canvas.ImageBitmap.Clip (EdgeStyle)
import Cork.Graphics.Canvas.ImageData.Immutable.Filters.Blur (Blur) as Blur
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale (Mode) as Grayscale
import Cork.Hashable (boundingBox, path, quadrilateral) as Cork.Hashable
import Cork.Sprites.Caches (Hash)
import Cork.Svg.Path (Path) as Svg
import Cork.Web.HTML.HTMLLoadedImageElement (Source) as HTMLLoadedImageElement
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Mu (Mu(..), roll)
import Data.Hashable (class Hashable, hash)
import Data.Hashable (hash) as Hashable
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Data.Tuple.Nested ((/\))
import Geometry.Plane (BoundingBox)
import Geometry.Plane.Figures.Polygons.Quadrilateral (Quadrilateral)
import Seegee.Geometry.Distance.Units (Pixel) as Units

data ImageDataF r
  = Blur Hash Blur.Blur r
  | ClipImageBitmap Hash (Svg.Path Units.Pixel) EdgeStyle (Maybe (BoundingBox Units.Pixel)) (ImageBitmapF r)
  | FromImageBitmap Hash (Maybe (BoundingBox Units.Pixel)) (ImageBitmapF r)
  | Grayscale Hash Grayscale.Mode r
  | GrayscaleToAlpha Hash Grayscale.Mode r
  | Project Hash (Quadrilateral Units.Pixel) (ImageBitmapF r)
  | StackedBlur Hash Int r
derive instance functorImageDataF ∷ Functor ImageDataF
derive instance eqImageDataF ∷ (Eq r) ⇒ Eq (ImageDataF r)
instance eq1ImageDataF ∷ Eq1 ImageDataF where
  eq1 = eq

instance foldableImageDataF ∷ Foldable ImageDataF where
  foldMap f (Blur _ _ r) = f r
  foldMap f (ClipImageBitmap _ _ _ _ i) = foldMap f i
  foldMap f (FromImageBitmap _ _ i) = foldMap f i
  foldMap f (Grayscale _ _ r) = f r
  foldMap f (GrayscaleToAlpha _ _ r) = f r
  foldMap f (Project _ _ r) = foldMap f r
  foldMap f (StackedBlur _ _ r) = f r

  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableImageDataF ∷ Traversable ImageDataF where
  sequence (Blur h b i) = Blur h b <$> i
  sequence (ClipImageBitmap h p e bb i) = ClipImageBitmap h p e bb <$> sequence i
  sequence (FromImageBitmap h bb i) = FromImageBitmap h bb <$> sequence i
  sequence (Grayscale h m i) = Grayscale h m <$> i
  sequence (GrayscaleToAlpha h m i) = GrayscaleToAlpha h m <$> i
  sequence (Project h q i) = Project h q <$> sequence i
  sequence (StackedBlur h r i) = StackedBlur h r <$> i

  traverse f = traverseDefault f

instance hashableImageDataF ∷ (Eq r) ⇒ Hashable (ImageDataF r) where
  hash (Blur h _ _) = h
  hash (ClipImageBitmap h _ _ _ _) = h
  hash (FromImageBitmap h _ _) = h
  hash (Grayscale h _ _) = h
  hash (GrayscaleToAlpha h _ _) = h
  hash (Project h _ _) = h
  hash (StackedBlur h _ _) = h

data ImageBitmapF r
  = ExternalImage Hash HTMLLoadedImageElement.Source
  | FromImageData Hash (ImageDataF r)
derive instance functorImageBitmapF ∷ Functor ImageBitmapF
derive instance eqImageBitmapF ∷ (Eq r) ⇒ Eq (ImageBitmapF r)
instance eq1ImageBitmapF ∷ Eq1 ImageBitmapF where
  eq1 = eq
instance hashableImageBitmapF ∷ (Eq r) ⇒ Hashable (ImageBitmapF r) where
  hash (ExternalImage h _) = h
  hash (FromImageData h _) = h

instance foldableImageBitmapF ∷ Foldable ImageBitmapF where
  foldMap f (ExternalImage _ _) = mempty
  foldMap f (FromImageData _ i) = foldMap f i

  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableImageBitmapF ∷ Traversable ImageBitmapF where
  sequence (ExternalImage h s) = pure (ExternalImage h s)
  sequence (FromImageData h i) = FromImageData h <$> sequence i

  traverse f = traverseDefault f

type ImageData = Mu ImageDataF
type ImageBitmap = ImageBitmapF ImageData

blur ∷ Blur.Blur → ImageData → ImageData
blur b t@(In i) = roll $ Blur (Hashable.hash ("blur" /\ b /\ i)) b t

clip ∷ Svg.Path Units.Pixel → EdgeStyle → Maybe (BoundingBox Units.Pixel) → ImageBitmap → ImageData
clip path e bb i = roll $ ClipImageBitmap h path e bb i
  where
    h = case bb of
      Nothing → hash ("clip" /\ Cork.Hashable.path path /\ i)
      Just bb' → hash ("clip" /\ Cork.Hashable.path path /\ Cork.Hashable.boundingBox bb' /\ i)

externalImageData ∷ HTMLLoadedImageElement.Source → Maybe (BoundingBox Units.Pixel) → ImageData
externalImageData url bb = fromImageBitmap bb (externalImage url)

fromImageBitmap ∷ Maybe (BoundingBox Units.Pixel) → ImageBitmap → ImageData
fromImageBitmap bb i = roll $ FromImageBitmap h bb i
  where
    h = case bb of
      Just bb' → hash (i /\ Cork.Hashable.boundingBox bb')
      Nothing → hash i

grayscale ∷ Grayscale.Mode → ImageData → ImageData
grayscale mode t@(In i) = roll $ Grayscale (Hashable.hash ("grayscale" /\ mode /\ i)) mode t

grayscaleToAlpha ∷ Grayscale.Mode → ImageData → ImageData
grayscaleToAlpha mode t@(In i) = roll $
  GrayscaleToAlpha (Hashable.hash ("grayscaleToAlpha" /\ mode /\ i)) mode t

stackedBlur ∷ Int → ImageData → ImageData
stackedBlur radius t@(In i) = roll $
  StackedBlur (Hashable.hash ("stackedBlur" /\ radius /\ i)) radius t

externalImage ∷ HTMLLoadedImageElement.Source → ImageBitmap
externalImage url = ExternalImage (hash url) url

fromImageData ∷ ImageData → ImageBitmap
fromImageData (In i) = FromImageData (hash i) i

project ∷ Quadrilateral Units.Pixel → ImageBitmap → ImageData
project quad i = roll $
  Project (Hashable.hash ("project" /\ Cork.Hashable.quadrilateral quad /\ i)) quad i

