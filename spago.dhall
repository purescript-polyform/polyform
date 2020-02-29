{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
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
}

