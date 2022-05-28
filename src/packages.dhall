let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220527/packages.dhall
        sha256:15dd8041480502850e4043ea2977ed22d6ab3fc24d565211acde6f8c5152a799

in  upstream
  with math =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/purescript/purescript-math"
  , version = "c052d221193d25255f6b9024df8df174e54dc550"
  }
  with easy-ffi =
  { dependencies = [ "prelude" ]
  , repo = "https://github.com/pelotom/purescript-easy-ffi"
  , version = "b8b6891277839167cd65af0857315acce0e9c6b6"
  }
  with purescript-three =
    { dependencies =
      [ "foreign", "web-dom", "easy-ffi", "math", "refs", "transformers" ]
    , repo = "https://github.com/anthonyquizon/purescript-three.git"
    , version = "da0ccd095bae3c539232269e7efc6a63d532030d"
    }
  with purescript-threejs =
    { dependencies = [ "prelude", "effect", "purescript-three" ]
    , repo = "https://github.com/dktr0/purescript-threejs"
    , version = "56e531d7acc3283f721f755021ff68b8502a4be6"
    }
