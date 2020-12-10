# purescript-cork

Simple experimental canvas renderer which reuses spork primitives and provides its own `App` type.

Still WIP - battle testing in the current project.

## Usage

`Cork.Sprites` is sprites rendering and caching app. I'm going to provide full working examples soon...

<!--
## Extensions

### Full DAG

It seems that there is possibility to introduce DAG support (multiple inputs into the node) by introducing state into the node and changing signature like:

type Plan = Map hash (Tuple (f i) (i → Effect (Plan hash f i j)))

### Extending plan types

```purescript
type Nodes = (a ∷ A, b ∷ B, c ∷ C)

type Plan Nodes = Map hash
    (Tuple
      (f (Variant Nodes))
      { a ∷ A → Plan Nodes
      , b ∷ B → Plan Nodes
      , c ∷ C → Plan Nodes
      }
    )
```

Is this worth anything? Maybe it is better to guard "type safety" by construction:

type Nodes = (a ∷ A, b ∷ B, c ∷ C)

``` purescript
-- This `lifts` a into `Variant / Sum` and dispatch result.
nodeA ∷ Tuple (f a) (a → Plan)
```

-->
