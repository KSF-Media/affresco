let A = ./AppServer.dhall

let AppServer = A.AppServer

in  AppServer::{
    , id = "app-article-server"
    , name = "App article server"
    , buildDir = "app-article-server"
    , deployDir = "app-article-server"
    , previewUrl = "article/c7da734f-9e2b-45be-b645-5f4742766486"
    , runtime = "nodejs12"
    , entrypoint = "node dist/server"
    , instance_class = Some "F4"
    , env = toMap
        { PERSONA_URL = "https://persona.staging.ksfmedia.fi/v1"
        , LETTERA_URL = "https://lettera.staging.ksfmedia.fi/v3"
        }
    }
