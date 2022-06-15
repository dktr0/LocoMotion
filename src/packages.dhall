let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220531/packages.dhall
        sha256:278d3608439187e51136251ebf12fabda62d41ceb4bec9769312a08b56f853e3

in  upstream
  with purescript-threejs =
    { dependencies = [ "prelude", "effect" ]
    , repo = "https://github.com/dktr0/purescript-threejs"
    , version = "d1f29fc72a2c98b815cf8bc517af0f6098541707"
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
    , version = "7d31a28e9de4014c4927844aed861254417f29e5"
    }
