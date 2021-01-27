{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "affresco-article"
, dependencies =
    [ "effect"
    , "aff"
    , "react-basic"
    , "psci-support"
    , "prelude"
    , "simple-json"
    , "now"
    , "affresco-components"
    , "affresco-user"
    , "uuid"
    ]
, packages =
    ../../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
