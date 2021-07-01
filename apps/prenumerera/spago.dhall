{ name = "prenumerera"
, dependencies =
  [ "effect"
  , "aff"
  , "prelude"
  , "affresco-components"
  , "affresco-user"
  , "react-basic"
  , "react-basic-hooks"
  , "routing"
  , "ordered-collections"
  , "debug"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
