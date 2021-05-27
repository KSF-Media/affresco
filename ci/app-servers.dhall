let Actions = ./workflows.dhall

let app-servers =
        [ Actions.AppServer::{
          , id = "app-article-server"
          , name = "App article server"
          , buildDir = "app-article-server"
          , deployDir = "app-article-server"
          }
        ]
      : List Actions.AppServer.Type

in  app-servers
