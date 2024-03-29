{ name =
    "affresco-components"
, dependencies =
    [ "effect"
    , "console"
    , "avar"
    , "aff"
    , "aff-promise"
    , "affjax-node"
    , "affjax-web"
    , "argonaut"
    , "argonaut-codecs"
    , "argonaut-core"
    , "formatters"
    , "datetime"
    , "react-basic"
    , "react-basic-classic"
    , "react-basic-dom"
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
    , "web-socket"
    ]
, packages =
    ../../packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
