{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "affresco"
, dependencies =
    [ "effect"
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
	]
, packages =
    ./packages.dhall
}
