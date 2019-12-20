{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "argonaut"
  , "argonaut-generic"
  , "canvas"
  , "console"
  , "debug"
  , "effect"
  , "geometry-utils"
  , "matryoshka"
  , "psci-support"
  , "pointed-list"
  , "seegee"
  , "spork"
  , "unordered-collections"
  , "unsafe-coerce"
  , "web-html"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
