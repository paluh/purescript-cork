let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211030/packages.dhall sha256:5cd7c5696feea3d3f84505d311348b9e90a76c4ce3684930a0ff29606d2d816c

let geometry-utils = mkPackage
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "group"
  , "integers"
  , "math"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "test-unit"
  , "typelevel"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
  "https://git@github.com/paluh/purescript-geometry-utils.git"
  "1e0a384e6fa7148d63aa00686a27f200dfbcb5d9"

let js-uri = mkPackage
  [ "assert", "effect", "functions", "maybe", "prelude" ]
  "https://github.com/srghma/purescript-js-uri.git"
  "d25d83390ba9cf948f46695b55a5511895b0068c"

let matryoshka = mkPackage
  [ "fixed-points"
  , "free"
  , "prelude"
  , "profunctor"
  , "transformers"
  ]
  "https://github.com/slamdata/purescript-matryoshka.git"
  "v0.4.0"

let pointed-list = mkPackage
  [ "arrays"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "psci-support"
  , "test-unit"
  ]
  "https://github.com/paluh/purescript-pointed-list.git"
  "abc5027ff4a73fa9e97c3aa5dd5bbce07e9a6fc2"

let smolder = mkPackage
  [ "bifunctors"
  , "catenable-lists"
  , "free"
  , "js-uri"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  ]
  "https://github.com/nsaunders/purescript-smolder.git"
  "ps-0.14"

let seegee = mkPackage
  [ "console", "effect", "geometry-utils" ]
  "ssh://git@github.com/paluh/purescript-seegee.git"
  "e0ca8c3e45655019b5f0c386f6c8c9f376caf5cf"
let js-unsafe-stringify = mkPackage
  ([] : List Text)
  "https://github.com/paluh/purescript-js-unsafe-stringify"
  "master"

in  upstream
  with
    geometry-utils = geometry-utils
  with
    js-unsafe-stringify = js-unsafe-stringify
  with
    js-uri = js-uri
  with
    matryoshka = matryoshka
  with
    pointed-list = pointed-list
  with
    seegee = seegee
  with
    smolder = smolder
