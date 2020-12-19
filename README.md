# purescript-cork

Simple *experimental* canvas renderer which reuses spork primitives and provides its own `App` type.

Still WIP - battle testing in the current project.

## Usage

`Cork.Sprites` is sprites rendering and caching app. A simple example can be found in _src/Example.purs_

## Credits

I've done a lot of `<Ctrl-C><Ctrl-V>` driven developement here. I've included most filters from _konva_. I've included perspective transformation from _paperprograms_ and of course I've used a lot of pieces (beside depending on it) from _spork_.

I hope that I've listed all authors in the LICENSE file correctly... Please inform if it that is not the case.


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
