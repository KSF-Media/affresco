{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mitt-konto"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "affresco-components"
  , "affresco-user"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "facebook"
  , "foldable-traversable"
  , "foreign-generic"
  , "formatters"
  , "integers"
  , "js-date"
  , "lists"
  , "maybe"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "react-basic-router"
  , "record"
  , "simple-json"
  , "strings"
  , "tuples"
  , "uuid"
  , "web-html"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
