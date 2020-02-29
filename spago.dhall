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
    , "pointed-list"
    , "psci-support"
    , "seegee"
    , "smolder"
    , "spork"
    , "stringutils"
    , "unordered-collections"
    , "unsafe-coerce"
    , "variant"
    , "web-html"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
