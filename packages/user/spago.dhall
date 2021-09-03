{ name = "affresco-user"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "affresco-components"
  , "argonaut-codecs"
  , "console"
  , "datetime"
  , "effect"
  , "facebook"
  , "foreign-generic"
  , "formatters"
  , "js-date"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "react-basic-hooks"
  , "react-basic-router"
  , "record"
  , "simple-json"
  , "uuid"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
