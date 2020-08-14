{ name = "vetrina-tests"
, dependencies =
  [ "aff", "console", "effect", "psci-support", "test-unit", "toppokki" ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
