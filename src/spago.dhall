{ name = "LocoMotion"
, dependencies =
  [ "arrays"
  , "console"
  , "datetime"
  , "easy-ffi"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
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
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
