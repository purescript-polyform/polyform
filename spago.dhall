{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, license = "BSD-3-Clause"
, name = "polyform"
, dependencies =
    [ "foreign"
    , "foreign-object"
    , "generics-rep"
    , "invariant"
    , "newtype"
    , "ordered-collections"
    , "profunctor"
    , "quickcheck-laws"
    , "run"
    , "transformers"
    , "validation"
    , "variant"
    ]
, packages = ../magusai/packages.dhall
, repository = "https://github.com/paluh/purescript-polyform.git"
}

