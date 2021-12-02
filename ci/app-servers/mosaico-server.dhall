let A = ./AppServer.dhall

let AppServer = A.AppServer

let Handler = A.Handler

in  AppServer::{
    , id = "mosaico-server"
    , name = "Mosaico server"
    , buildDir = "mosaico"
    , deployDir = "mosaico"
    , previewUrl = "artikel/c7da734f-9e2b-45be-b645-5f4742766486"
    , runtime = "nodejs12"
    , domains = [ "mosaico.app.ksfmedia.fi/*" ]
    , handlers =
      [ Handler::{ url = Some "/assets", static_dir = Some "dist/client" } ]
    , entrypoint =
        ''
          node -r dotenv/config -e "require('./output/Main/index').main()"
        ''
    , env = toMap
        { LETTERA_URL = "https://lettera.api.ksfmedia.fi/v4beta"
        }
    }
