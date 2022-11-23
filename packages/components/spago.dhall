{ name = "affresco-components"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax-node"
  , "affjax-web"
  , "argonaut"
  , "avar"
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
  , "quickcheck"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "read"
  , "simple-json"
  , "uuid"
  , "validation"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
