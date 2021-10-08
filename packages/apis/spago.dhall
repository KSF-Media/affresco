{ name = "affresco-apis"
, dependencies =
    [ "effect"
    , "console"
    , "avar"
    , "aff"
    , "aff-promise"
    , "simple-ajax"
    , "formatters"
    , "datetime"
    , "psci-support"
    , "prelude"
    , "js-date"
    , "foreign-generic"
    , "ordered-collections"
    , "simple-json"
    , "facebook"
    , "now"
    , "validation"
    , "read"
    , "uuid"
    , "quickcheck"
    , "web-url"
    ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
