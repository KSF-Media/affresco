{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mosaico"
, dependencies =
  [ "aff"
  , "affjax"
  , "affresco-components"
  , "affresco-user"
  , "affresco-vetrina"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-http"
  , "nullable"
  , "ordered-collections"
  , "payload"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "record"
  , "routing"
  , "simple-json"
  , "strings"
  , "tuples"
  , "uuid"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
