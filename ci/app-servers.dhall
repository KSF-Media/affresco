{-

servers: Apps to be deployed to AppEngine
all: list of all servers

Note: When adding a new server please also update the list of all servers

-}
let Actions = ./workflows.dhall

let servers =
      { app-article-server = Actions.AppServer::{
        , id = "app-article-server"
        , name = "App article server"
        , buildDir = "app-article-server"
        , deployDir = "app-article-server"
        , previewUrl = "article/c7da734f-9e2b-45be-b645-5f4742766486"
        , runtime = "nodejs12"
        , entrypoint = "node dist/server"
        }
      , mosaico = Actions.AppServer::{
        , id = "mosaico-server"
        , name = "Mosaico server"
        , buildDir = "mosaico"
        , deployDir = "mosaico"
        , previewUrl = "artikel/c7da734f-9e2b-45be-b645-5f4742766486"
        , runtime = "nodejs12"
        , entrypoint =
            ''
              node -e "require('./output/Main/index').main()"
            ''
        }
      }

let all = [ servers.app-article-server, servers.mosaico ]

in  { servers, all }
