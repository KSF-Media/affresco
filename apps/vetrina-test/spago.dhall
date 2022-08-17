{ name = "vetrina-test"
, dependencies =
  [ "aff"
  , "affresco-components"
  , "affresco-test"
  , "affresco-vetrina"
  , "console"
  , "effect"
  , "prelude"
  , "react-basic"
  , "react-basic-classic"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
