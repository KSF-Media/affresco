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
    , "react-basic-classic"
    , "react-basic-dom"
    , "react-basic-hooks"
    , "react-basic-router"
    , "psci-support"
    , "prelude"
    , "js-date"
    , "foreign-generic"
    , "ordered-collections"
    , "simple-json"
    , "facebook"
    , "now"
    , "web-html"
	, "uuid"
    ]
, packages =
    ../../packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
