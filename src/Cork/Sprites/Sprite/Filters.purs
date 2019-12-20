module Cork.Sprites.Sprite.Filters where

import Prelude

import Cork.Data.Functor.Variant.Mu (MuVariantF, inj)
import Cork.Sprites.Cache (Hash)
import Cork.Sprites.Cache (Hash) as Cache
import Data.Functor.Mu (Mu)
import Data.Functor.Variant (FProxy, VariantF, on)
import Data.Hashable (hash) as Hashable
import Data.Tuple (Tuple(..))
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

data GrayscaleF r = GrayscaleF Hash r
derive instance functorGrayscaleF ∷ Functor GrayscaleF

data StackedBlurF r = StackedBlurF { hash ∷ Int, radius ∷ Int } r
derive instance functorStackedBlurF ∷ Functor StackedBlurF

type GrayscaleRow r = (grayscale ∷ FProxy GrayscaleF | r)
_grayscale = SProxy ∷ SProxy "grayscale"

type StackedBlurRow r = (stackedBlur ∷ FProxy StackedBlurF | r)
_stackedBlur = SProxy ∷ SProxy "stackedBlur"

type FiltersRow r = GrayscaleRow + StackedBlurRow + r
type FiltersF r = VariantF (FiltersRow r)
type Filters r = Mu (VariantF (FiltersRow + r))

grayscale :: forall r. (MuVariantF (GrayscaleRow + r) → Cache.Hash) → MuVariantF (GrayscaleRow + r) → MuVariantF (GrayscaleRow + r)
grayscale h t = inj _grayscale (GrayscaleF (Hashable.hash (Tuple "grayscale" (h t))) t)

stackedBlur :: forall r. (MuVariantF (StackedBlurRow + r) → Cache.Hash) → Int → MuVariantF (StackedBlurRow + r) → MuVariantF (StackedBlurRow + r)
stackedBlur h radius t = inj _stackedBlur (StackedBlurF { hash: Hashable.hash (Tuple ("stackedblur:" <> show radius) (h t)), radius } t)

hash ∷ ∀ a r. (VariantF r a → Cache.Hash) → FiltersF r a → Cache.Hash
hash f = f
  # on _grayscale (\(GrayscaleF h _) → h)
  # on _stackedBlur (\(StackedBlurF { hash: h } _) → h)
