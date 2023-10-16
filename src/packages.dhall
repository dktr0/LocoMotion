let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231003/packages.dhall
        sha256:dccca0d661a634bfe39ad7abcb52fbd938d5b2e28322d2954964cbb7c145aa81

in  upstream
  with purescript-threejs =
    { dependencies = [ "prelude", "effect" ]
    , repo = "https://github.com/dktr0/purescript-threejs"
    , version = "acd7a42d475cfeb499e925fcf7227a9f00d7f5bf"
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
    , version = "26f206aeda7ba0e263af107e33fcc69c1f929d47"
    }
