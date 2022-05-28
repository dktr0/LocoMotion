{ name = "LocoMotion"
, dependencies =
  [ "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "numbers"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "purescript-threejs"
  , "refs"
  , "strings"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
