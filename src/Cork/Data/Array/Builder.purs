module Cork.Data.Array.Builder where

import Prelude

newtype Builder a = Builder (Array a → Array a)
instance semigroupBuilder ∷ Semigroup (Builder a) where
  append (Builder b1) (Builder b2) = Builder (b1 <<< b2)
instance monoidBuilder ∷ Monoid (Builder a) where
  mempty = Builder identity

foreign import unsafeCons ∷ ∀ a. a → Array a → Array a
foreign import unsafeSnoc ∷ ∀ a. a → Array a → Array a

cons ∷ ∀ a. a → Builder a
cons a = Builder (unsafeCons a)

snoc ∷ ∀ a. a → Builder a
snoc a = Builder (unsafeSnoc a)

build ∷ ∀ a. Builder a → Array a
build (Builder f) = f []
