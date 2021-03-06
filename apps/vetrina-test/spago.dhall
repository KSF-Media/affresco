{ name = "vetrina-test"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affresco-components"
  , "affresco-test"
  , "affresco-user"
  , "affresco-vetrina"
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
  , "react-basic-classic"
  , "react-basic-dom"
  , "react-basic-router"
  , "simple-json"
  , "test-unit"
  , "toppokki"
  , "uuid"
  , "affjax"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
