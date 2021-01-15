{ name =
    "affresco-user"
, dependencies =
    [ "effect"
    , "record"
    , "console"
    , "aff"
    , "aff-promise"
    , "formatters"
    , "datetime"
    , "react-basic"
    , "react-basic-hooks"
    , "react-basic-router"
    , "psci-support"
    , "prelude"
    , "js-date"
    , "foreign-generic"
    , "generics-rep"
    , "ordered-collections"
    , "simple-json"
    , "facebook"
    , "now"
    , "affresco-components"
    , "uuid"
    ]
, packages =
    ../../packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
