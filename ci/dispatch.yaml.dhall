let Prelude = ./Prelude.dhall

let Env = < Staging | Production >

let Actions = ./workflows.dhall

let apps = ./app-servers.dhall

let Dispatch =
      { Type = { url : Optional Text, service : Optional Text }
      , default = { url = None Text, service = None Text }
      }

let mkDispatchYaml =
      \(env : Env) ->
      \(app : Actions.AppServer.Type) ->
        Dispatch::{
        , url =
            merge
              { Staging = Some "${app.id}.app-staging.ksfmedia.fi"
              , Production = Some "${app.id}.app.ksfmedia.fi"
              }
              env
        , service = Some app.id
        }

let generate =
      \(env : Env) ->
        { dispatch =
            Prelude.List.map
              Actions.AppServer.Type
              Dispatch.Type
              (mkDispatchYaml env)
              apps
        }

in  generate
