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
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "js-uri"
  , "lists"
  , "maybe"
  , "newtype"
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
  , "semirings"
  , "simple-json"
  , "strings"
  , "tuples"
  , "unordered-collections"
  , "uuid"
  , "validation"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
