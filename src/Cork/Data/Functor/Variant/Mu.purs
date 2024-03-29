module Cork.Data.Functor.Variant.Mu where

import Prelude

import Data.Functor.Mu (Mu, roll)
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant (inj) as Variant
import Data.Symbol (class IsSymbol)
import Matryoshka (Algebra)
import Prim.Row (class Cons) as Row
import Type.Prelude (Proxy)

type MuVariantF r = Mu (VariantF r)

inj ∷ ∀ f s a b. Row.Cons s f a b ⇒ IsSymbol s ⇒ Functor f ⇒ Proxy s → Algebra f (MuVariantF b)
inj label = roll <<< (Variant.inj label)
