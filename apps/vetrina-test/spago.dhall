{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ "affresco-components"
    , "affresco-user"
    , "affresco-vetrina"
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
	, "uuid"
    ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
