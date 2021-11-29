{-
AppServer Type
-}
let Prelude = ../Prelude.dhall

let Map = Prelude.Map.Type

let Handler =
      { Type = { url : Optional Text, static_dir : Optional Text }
      , default = { url = None Text, static_dir = None Text }
      }

let AppServer =
      { Type =
          { id : Text
          , buildDir : Text
          , deployDir : Text
          , name : Text
          , runtime : Text
          , entrypoint : Text
          , env : Map Text Text
          , previewUrl : Text
          , lockfile : Optional Text
          , caches : Optional Text
          , domains : List Text
          , handlers : List Handler.Type
          , instance_class : Optional Text
          }
      , default =
        { env = [] : Map Text Text
        , previewUrl = ""
        , lockfile = None Text
        , caches = None Text
        , domains = [] : List Text
        , handlers = [] : List Handler.Type
        , instance_class = None Text
        }
      }

in  { AppServer, Handler }
