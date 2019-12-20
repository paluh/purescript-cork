module Cork.Sprites.Cache.Machine.Plan where

import Prelude

import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Map (Map)
import Data.Map (pop, unionWith) as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..), fst)

newtype PlanD i hash o = PlanD (Map hash (Tuple o (i → PlanD i hash o)))

instance functorPlanD ∷ Functor (PlanD i h) where
  map f (PlanD p) = PlanD $ map (f *** map (map f)) p

instance functorWithIndexPlanD ∷ FunctorWithIndex hash (PlanD i hash) where
  mapWithIndex f (PlanD p) = PlanD (mapWithIndex s p)
    where
      s hash (Tuple o cont) = Tuple (f hash o) (map (mapWithIndex f) cont)

instance semigroupPlanD ∷ Ord hash ⇒ Semigroup (PlanD i hash o) where
  append (PlanD p1) (PlanD p2) = PlanD (Map.unionWith union p1 p2)
    where
      union (Tuple i cont1) (Tuple _ cont2) = Tuple i (append <$> cont1 <*> cont2)

instance monoidPlanD ∷ Ord hash ⇒ Monoid (PlanD i hash o) where
  mempty = PlanD mempty

type Plan f hash a = PlanD a hash (f a)

step ∷ ∀ a hash m. Ord hash ⇒ hash → a → Plan m hash a → { subplan ∷ Plan m hash a, plan ∷ Plan m hash a }
step hash a b@(PlanD p) = case Map.pop hash p of
  Just (Tuple (Tuple _ cont) p') → let s = cont a in { subplan: s, plan: s <> PlanD p' }
  Nothing → { subplan: mempty, plan: b }

plan :: forall a f h. Map h (Tuple (f a) (a -> Plan f h a)) -> Plan f h a
plan m = PlanD m

layerFoldMapWithIndex ∷ ∀ a f h m. Monoid m ⇒ (h → (f a) → m) → Plan f h a → m
layerFoldMapWithIndex f (PlanD p) = foldMapWithIndex (\h t → f h (fst t)) p
