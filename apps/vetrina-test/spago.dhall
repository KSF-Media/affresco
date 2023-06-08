{ name = "vetrina-test"
, dependencies =
  [ "aff"
  , "affresco-components"
  , "affresco-test"
  , "affresco-user"
  , "affresco-vetrina"
  , "console"
  , "effect"
  , "either"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
