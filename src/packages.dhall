let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231003/packages.dhall
        sha256:dccca0d661a634bfe39ad7abcb52fbd938d5b2e28322d2954964cbb7c145aa81

in  upstream
  with purescript-threejs =
    { dependencies = [ "prelude", "effect", "arraybuffer-types" ]
    , repo = "https://github.com/dktr0/purescript-threejs"
    , version = "f1abee9dbb77a69b6e9d27b82cd07a0a036eff4e"
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
    , version = "9329e3eac9e971af937c301a5c76d1fa419d8693"
    }
