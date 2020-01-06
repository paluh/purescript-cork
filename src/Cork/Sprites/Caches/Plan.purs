module Cork.Sprites.Caches.Plan where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (null) as Foldable
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map (Map)
import Data.Map (pop, unionWith) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)

data Plan hash f i j = Plan
  (Map hash (Tuple (f i) (i → Plan hash f i j)))
  (Map hash (Tuple (f j) (j → Plan hash f i j)))

instance semigroupPlan ∷ Ord hash ⇒ Semigroup (Plan hash f i j) where
  append (Plan pi1 pj1) (Plan pi2 pj2) = Plan (Map.unionWith union pi1 pi2) (Map.unionWith union pj1 pj2)
    where
      union
        ∷ ∀ k
        . Tuple (f k) (k → Plan hash f i j)
        → Tuple (f k) (k → Plan hash f i j)
        → Tuple (f k) (k → Plan hash f i j)
      union (Tuple i cont1) (Tuple _ cont2) = Tuple i (append <$> cont1 <*> cont2)

instance monoidPlan ∷ Ord hash ⇒ Monoid (Plan hash f i j) where
  mempty = Plan mempty mempty

null ∷ ∀ hash f i j. Plan hash f i j → Boolean
null (Plan pi pj) = Foldable.null pi && Foldable.null pj

step ∷ ∀ f hash i j. Ord hash ⇒ hash → Either i j → Plan hash f i j → { subplan ∷ Plan hash f i j, plan ∷ Plan hash f i j }
step hash v p@(Plan pi pj) = case v of
  Left i → case Map.pop hash pi of
    Just (Tuple (Tuple _ cont) pi') →
      let s = cont i in { subplan: s, plan: s <> Plan pi' pj }
    Nothing → { subplan: mempty, plan: p }
  Right j → case Map.pop hash pj of
    Just (Tuple (Tuple _ cont) pj') →
      let s = cont j in { subplan: s, plan: s <> Plan pi pj' }
    Nothing → { subplan: mempty, plan: p }

layerFoldMapWithIndex
  ∷ ∀ f h i j m
  . Monoid m
  ⇒ (h → (f i) → m)
  → (h → (f j) → m)
  → Plan h f i j
  → m
layerFoldMapWithIndex gi gj (Plan pi pj)
  = foldMapWithIndex (\h i → gi h (fst i)) pi
  <> foldMapWithIndex (\h j → gj h (fst j)) pj
