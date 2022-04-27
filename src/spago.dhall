{ name = "LocoMotion"
, dependencies =
  [ "arrays"
  , "console"
  , "datetime"
  , "easy-ffi"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "now"
  , "numbers"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "purescript-three"
  , "purescript-threejs"
  , "refs"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
