{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "mitt-konto"
, dependencies =
    [ "affresco-components"
    , "affresco-login"
    , "effect"
    , "console"
    , "aff"
    , "aff-promise"
    , "formatters"
    , "datetime"
    , "react-basic"
    , "psci-support"
    , "prelude"
    , "js-date"
    , "foreign-generic"
    , "generics-rep"
    , "ordered-collections"
    , "simple-json"
    , "facebook"
    , "now"
    ]
, packages =
    ../../packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
