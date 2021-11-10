{-

servers: Apps to be deployed to AppEngine
all: list of all servers

Note: When adding a new server please also update the list of all servers
TODO: Figure out a way to generate all from servers.

-}
let Prelude = ./Prelude.dhall

let Map = Prelude.Map.Type

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
          }
      , default =
        { env = [] : Map Text Text
        , previewUrl = ""
        , lockfile = None Text
        , caches = None Text
        , domains = [] : List Text
        }
      }

let servers =
      { app-article-server = AppServer::{
        , id = "app-article-server"
        , name = "App article server"
        , buildDir = "app-article-server"
        , deployDir = "app-article-server"
        , previewUrl = "article/c7da734f-9e2b-45be-b645-5f4742766486"
        , runtime = "nodejs12"
        , entrypoint = "node dist/server"
        }
      , mosaico = AppServer::{
        , id = "mosaico-server"
        , name = "Mosaico server"
        , buildDir = "mosaico"
        , deployDir = "mosaico"
        , previewUrl = "artikel/c7da734f-9e2b-45be-b645-5f4742766486"
        , runtime = "nodejs12"
        , domains = [ "mosaico.app.ksfmedia.fi/*" ]
        , entrypoint =
            ''
              node -e "require('./output/Main/index').main()"
            ''
        }
      }

let all = [ servers.app-article-server, servers.mosaico ]

in  { servers, all, AppServer }
