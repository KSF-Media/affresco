{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "kort"
, dependencies =
  [ "aff"
  , "affresco-components"
  , "affresco-user"
  , "arrays"
  , "avar"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
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
  , "tuples"
  , "web-html"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
