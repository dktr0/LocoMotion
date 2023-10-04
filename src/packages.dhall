let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231003/packages.dhall
        sha256:dccca0d661a634bfe39ad7abcb52fbd938d5b2e28322d2954964cbb7c145aa81

in  upstream
  with purescript-threejs =
    { dependencies = [ "prelude", "effect" ]
    , repo = "https://github.com/dktr0/purescript-threejs"
    , version = "9679234f3b6e17a49390284947d2312812f43c95"
    }
  with purescript-tempi =
    { dependencies =
      [ "console"
      , "datetime"
      , "effect"
      , "integers"
      , "maybe"
      , "newtype"
      , "now"
      , "partial"
      , "prelude"
      , "psci-support"
      , "rationals"
      ]
    , repo = "https://github.com/dktr0/purescript-tempi"
    , version = "7faa5fe921fc2273339ac53d5d1296d6e83ad5fd"
    }
