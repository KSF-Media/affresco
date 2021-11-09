let Prelude = ./Prelude.dhall

let Env = < Staging | Production >

let Actions = ./workflows.dhall

let apps = ./app-servers.dhall

let Dispatch =
      { Type = { url : Optional Text, service : Optional Text}
      , default = { url = None Text, service = None Text}
      }

let mkDispatch =
      \(appid : Text) ->
      \(domain : Text) ->
        Dispatch::{ service = Some appid, url = Some domain }

let mkDispatchYaml =
      \(env : Env) ->
      \(app : Actions.AppServer.Type) ->
        merge
          { Production =
                Prelude.List.map
                  Text
                  Dispatch.Type
                  (mkDispatch app.id)
                  app.domains
              # [ Dispatch::{
                  , service = Some app.id
                  , url = Some "${app.id}.app.ksfmedia.fi/*"
                  }
                ]
          , Staging =
            [ Dispatch::{
              , service = Some app.id
              , url = Some "${app.id}.app-staging.ksfmedia.fi/*"
              }
            ]
          }
          env

let generate =
      \(env : Env) ->
        { dispatch =
            Prelude.List.concatMap
              Actions.AppServer.Type
              Dispatch.Type
              (mkDispatchYaml env)
              apps.all
        }

in  generate
