let Actions = ./workflows.dhall

let app-servers =
        [ Actions.AppServer::{
          , id = "app-article-server"
          , name = "App article server"
          , buildDir = "app-article-server"
          , deployDir = "app-article-server"
          , previewUrl = "article/c7da734f-9e2b-45be-b645-5f4742766486"
          , runtime = "nodejs12"
          , entrypoint = "node dist/server"
          }
        , Actions.AppServer::{
          , id = "mosaico-server"
          , name = "Mosaico server"
          , buildDir = "mosaico-server"
          , deployDir = "mosaico-server"
          , previewUrl = "artikel/c7da734f-9e2b-45be-b645-5f4742766486"
          , runtime = "nodejs12"
          , entrypoint =
              ''
                node -e "require('./output/Main/index').main()"
              ''
          }
        ]
      : List Actions.AppServer.Type

in  app-servers
