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
  , "test-unit"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
