module Cork.Sprites.Sprite.Filters where

import Prelude

import Cork.Data.Functor.Variant.Mu (MuVariantF, inj)
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale (Mode, serMode) as Grayscale
import Cork.Sprites.Caches (Hash)
import Cork.Sprites.Caches (Hash) as Cache
import Data.Functor.Mu (Mu)
import Data.Functor.Variant (FProxy, VariantF, on)
import Data.Hashable (hash) as Hashable
import Data.Tuple (Tuple(..))
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

data GrayscaleF r = GrayscaleF { hash ∷ Hash, mode ∷ Grayscale.Mode } r
derive instance functorGrayscaleF ∷ Functor GrayscaleF

data GrayscaleToAlphaF r = GrayscaleToAlphaF { hash ∷ Hash, mode ∷ Grayscale.Mode } r
derive instance functorGrayscaleToAlphaF ∷ Functor GrayscaleToAlphaF

data StackedBlurF r = StackedBlurF { hash ∷ Int, radius ∷ Int } r
derive instance functorStackedBlurF ∷ Functor StackedBlurF

type GrayscaleRow r = (grayscale ∷ FProxy GrayscaleF | r)
_grayscale = SProxy ∷ SProxy "grayscale"

type GrayscaleToAlphaRow r = (grayscaleToAlpha ∷ FProxy GrayscaleToAlphaF | r)
_grayscaleToAlpha = SProxy ∷ SProxy "grayscaleToAlpha"

type StackedBlurRow r = (stackedBlur ∷ FProxy StackedBlurF | r)
_stackedBlur = SProxy ∷ SProxy "stackedBlur"

type FiltersRow r = GrayscaleRow + GrayscaleToAlphaRow + StackedBlurRow + r
type FiltersF r = VariantF (FiltersRow r)
type Filters r = Mu (VariantF (FiltersRow + r))

grayscale ∷ ∀ r. (MuVariantF (GrayscaleRow + r) → Cache.Hash) → Grayscale.Mode → MuVariantF (GrayscaleRow + r) → MuVariantF (GrayscaleRow + r)
grayscale h mode t =
  inj _grayscale (GrayscaleF { hash: Hashable.hash (Tuple ("grayscale-" <> Grayscale.serMode mode) (h t)), mode } t)

grayscaleToAlpha ∷ ∀ r. (MuVariantF (GrayscaleToAlphaRow + r) → Cache.Hash) → Grayscale.Mode → MuVariantF (GrayscaleToAlphaRow + r) → MuVariantF (GrayscaleToAlphaRow + r)
grayscaleToAlpha h mode t =
  inj _grayscaleToAlpha (GrayscaleToAlphaF { hash: Hashable.hash (Tuple ("grayscale-toAlpha-" <> Grayscale.serMode mode) (h t)), mode } t)

stackedBlur ∷ ∀ r. (MuVariantF (StackedBlurRow + r) → Cache.Hash) → Int → MuVariantF (StackedBlurRow + r) → MuVariantF (StackedBlurRow + r)
stackedBlur h radius t = inj _stackedBlur (StackedBlurF { hash: Hashable.hash (Tuple ("stackedblur:" <> show radius) (h t)), radius } t)

hash ∷ ∀ a r. (VariantF r a → Cache.Hash) → FiltersF r a → Cache.Hash
hash f = f
  # on _grayscale (\(GrayscaleF { hash: h } _) → h)
  # on _grayscaleToAlpha (\(GrayscaleToAlphaF { hash: h } _) → h)
  # on _stackedBlur (\(StackedBlurF { hash: h } _) → h)
