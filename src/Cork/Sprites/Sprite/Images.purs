module Cork.Sprites.Sprite.Images where

import Prelude

import Cork.Data.Functor.Variant.Mu (MuVariantF, inj)
import Cork.Sprites.Cache (Hash)
import Cork.Web.HTML.HTMLLoadedImageElement (Source(..))
import Cork.Web.HTML.HTMLLoadedImageElement (Source) as HTMLLoadedImageElement
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Functor.Variant (FProxy, VariantF, on)
import Data.Hashable (hash) as Hashable
import Data.Traversable (class Traversable, traverseDefault)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

data ExternalImageF r = ExternalImageF { hash ∷ Hash, url ∷ HTMLLoadedImageElement.Source }
derive instance functorExternalImageF ∷ Functor ExternalImageF
instance foldableExternalImageF ∷ Foldable ExternalImageF where
  foldMap f (ExternalImageF _) = mempty

  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableExternalImageF ∷ Traversable ExternalImageF where
  sequence e@(ExternalImageF s) = pure (ExternalImageF s)

  traverse f = traverseDefault f

type ExternalImageRow r = (externalImage ∷ FProxy ExternalImageF | r)
_externalImage = SProxy ∷ SProxy "externalImage"

type ImagesRow r = ExternalImageRow + r
type ImagesF r = VariantF (ImagesRow + r)
type Images r = MuVariantF (ImagesRow + r)

hashSource ∷ HTMLLoadedImageElement.Source → Int
hashSource (DataURL d) = Hashable.hash d
hashSource (URL url) = Hashable.hash url

externalImage :: forall r. HTMLLoadedImageElement.Source → MuVariantF (ExternalImageRow + r)
externalImage url = inj _externalImage (ExternalImageF { hash: hashSource url, url })

hash ∷ ∀ a r. (VariantF r a → Hash) → ImagesF r a → Hash
hash f = f # on _externalImage (\(ExternalImageF { hash: h }) → h)
