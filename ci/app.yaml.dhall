let Prelude = ./Prelude.dhall

let AS = ./app-servers.dhall

let T = ./app-servers/AppServer.dhall

let Map = Prelude.Map.Type

let AppYaml =
      { Type =
          { runtime : Optional Text
          , service : Optional Text
          , entrypoint : Optional Text
          , handlers : List T.Handler.Type
          , env_variables : Map Text Text
          }
      , default =
        { runtime = Some "nodejs12"
        , service = None Text
        , entrypoint = None Text
        , handlers = [] : List T.Handler.Type
        , env_variables = [] : Map Text Text
        }
      }

let generate =
      \(app : T.AppServer.Type) ->
        AppYaml::{
        , runtime = Some app.runtime
        , service = Some app.id
        , entrypoint = Some app.entrypoint
        , handlers = app.handlers
        , env_variables = app.env
        }

in  generate
