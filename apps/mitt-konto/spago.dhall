{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "mitt-konto"
, dependencies =
    [ "affresco-components"
    , "affresco-user"
    , "effect"
    , "console"
    , "aff"
    , "aff-promise"
    , "formatters"
    , "datetime"
    , "react-basic"
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
	, "uuid"
    ]
, packages =
    ../../packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
