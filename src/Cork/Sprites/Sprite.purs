module Cork.Sprites.Sprite where

import Prelude

import Cork.Data.Functor.Variant.Mu (MuVariantF)
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale (Mode) as Grayscale
import Cork.Sprites.Caches (Hash)
import Cork.Sprites.Sprite.Filters (FiltersRow)
import Cork.Sprites.Sprite.Filters (grayscale, grayscaleToAlpha, hash, stackedBlur) as Filters
import Cork.Sprites.Sprite.Images (ExternalImageRow)
import Cork.Sprites.Sprite.Images (hash) as Images
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Variant (VariantF, case_)
import Type.Row (type (+))

type SpriteF = VariantF (ExternalImageRow + FiltersRow + ())
type Sprite = MuVariantF (ExternalImageRow + FiltersRow + ())

hash ∷ Sprite → Hash
hash (Mu.In s) = (case_ # Images.hash # Filters.hash) s

grayscale ∷ Grayscale.Mode → Sprite → Sprite
grayscale mode = Filters.grayscale hash mode

grayscaleToAlpha ∷ Grayscale.Mode → Sprite → Sprite
grayscaleToAlpha mode = Filters.grayscaleToAlpha hash mode

stackedBlur ∷ Int → Sprite → Sprite
stackedBlur = Filters.stackedBlur hash

