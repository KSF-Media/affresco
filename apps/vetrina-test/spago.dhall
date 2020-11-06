{ name = "vetrina-test"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affresco-components"
  , "affresco-user"
  , "affresco-vetrina"
  , "console"
  , "datetime"
  , "effect"
  , "facebook"
  , "foreign-generic"
  , "formatters"
  , "generics-rep"
  , "js-date"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "simple-json"
  , "test-unit"
  , "toppokki"
  , "uuid"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
