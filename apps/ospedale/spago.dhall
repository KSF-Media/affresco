{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mitt-konto"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "affresco-components"
  , "affresco-user"
  , "argonaut"
  , "arrays"
  , "avar"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "form-urlencoded"
  , "http-methods"
  , "maybe"
  , "newtype"
  , "now"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "routing"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "uuid"
  , "web-events"
  , "web-html"
  , "web-socket"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
