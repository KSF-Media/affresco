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
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
