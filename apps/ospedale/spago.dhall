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
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "maybe"
  , "newtype"
  , "now"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "routing"
  , "strings"
  , "tuples"
  , "uuid"
  , "web-html"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
