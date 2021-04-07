{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mosaico"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "affresco-components"
  , "affresco-user"
  , "console"
  , "datetime"
  , "effect"
  , "facebook"
  , "foreign-generic"
  , "formatters"
  , "js-date"
  , "now"
  , "ordered-collections"
  , "payload"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "react-basic-dom"
  , "simple-json"
  , "uuid"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
