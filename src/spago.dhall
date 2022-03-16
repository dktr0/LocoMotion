
{ name = "LocoMotion"
, dependencies =
  [ "arrays"
  , "console"
  , "easy-ffi"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "purescript-three"
  , "refs"
  , "web-uievents"
  , "purescript-threejs"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
