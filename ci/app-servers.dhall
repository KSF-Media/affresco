let Actions = ./workflows.dhall

let app-servers =
        [ Actions.AppServer::{
          , id = "app-article-server"
          , name = "App article server"
          , buildDir = "app-article-server"
          , deployDir = "app-article-server"
          , previewUrl = "article/c7da734f-9e2b-45be-b645-5f4742766486"
          }
        ]
      : List Actions.AppServer.Type

in  app-servers
