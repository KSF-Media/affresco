{ name = "vetrina-test"
, dependencies =
  [ "aff"
  , "affresco-components"
  , "affresco-test"
  , "affresco-vetrina"
  , "console"
  , "effect"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "react-basic-classic"
  , "strings-extra"
  , "test-unit"
  , "unordered-collections"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
