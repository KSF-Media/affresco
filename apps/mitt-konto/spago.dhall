{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mitt-konto"
, dependencies =
  [ "aff"
  , "affresco-apis"
  , "affresco-components"
  , "affresco-user"
  , "argonaut-codecs"
  , "arrays"
  , "avar"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
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
  , "record"
  , "routing"
  , "routing-duplex"
  , "strings"
  , "transformers"
  , "tuples"
  , "uuid"
  , "validation"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
