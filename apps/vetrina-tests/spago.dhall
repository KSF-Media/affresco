{ name = "vetrina-tests"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "formatters"
  , "now"
  , "psci-support"
  , "test-unit"
  , "toppokki"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
