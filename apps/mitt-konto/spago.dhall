{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mitt-konto"
, dependencies =
  [ "aff"
  , "affresco-components"
  , "affresco-user"
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
  , "newtype"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "react-basic"
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
  , "web-html"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
