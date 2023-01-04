let mkPackage =
  https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221201/packages.dhall
    sha256:d1a68fa15709eaa686515eb5b9950d82c743f7bf73e3d87a4abe9e1be6fda571

in  upstream
  with
    js-unsafe-stringify =
      { dependencies = ([] : List Text)
      , repo = "https://github.com/paluh/purescript-js-unsafe-stringify.git"
      , version = "v0.2.0"
      }
