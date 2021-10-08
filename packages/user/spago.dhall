{ name = "affresco-user"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "affresco-apis"
  , "affresco-components"
  , "argonaut-codecs"
  , "arrays"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "facebook"
  , "foldable-traversable"
  , "foreign-generic"
  , "foreign-object"
  , "formatters"
  , "integers"
  , "js-date"
  , "lists"
  , "maybe"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "react-basic-router"
  , "record"
  , "simple-json"
  , "strings"
  , "transformers"
  , "unsafe-coerce"
  , "uuid"
  , "validation"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
