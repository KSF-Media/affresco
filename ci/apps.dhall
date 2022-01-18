let Prelude = ./Prelude.dhall

let Map = Prelude.Map.Type

let App =
      { Type =
          { buildDir : Text
          , deployDir : Text
          , name : Text
          , env : Map Text Text
          , lockfile : Optional Text
          , caches : Optional Text
          }
      , default =
        { env = [] : Map Text Text, lockfile = None Text, caches = None Text }
      }

let apps =
        [ App::{ name = "Scripts", buildDir = "scripts", deployDir = "scripts" }
        , App::{
          , name = "Mitt Konto"
          , buildDir = "mitt-konto"
          , deployDir = "mitt-konto"
          , env = toMap
              { PRODUCTION_SENTRY_DSN =
                  "https://54e59357e2fd42db917041739865e2c9@sentry.io/5174203"
              , MAINTENANCE_MODE = "false"
              }
          , lockfile = Some "yarn.lock"
          , caches = Some
              ''
              apps/mitt-konto/.spago
              apps/mitt-konto/output
              ''
          }
        , App::{
          , name = "Vetrina (for testing only)"
          , buildDir = "vetrina-test"
          , deployDir = "vetrina-test"
          , env = toMap
              { NODE_ENV = "development"
              , SENTRY_DSN =
                  "https://6479d7c55fbd4e0db7d9ac755083865f@sentry.io/3718842"
              }
          , lockfile = Some "yarn.lock"
          , caches = Some
              ''
              apps/vetrina-test/.spago
              apps/vetrina-test/output
              ''
          }
        , App::{
          , name = "Elections (EU)"
          , buildDir = "elections"
          , deployDir = "elections-eu"
          , env = toMap
              { ELECTION_BACKEND_URL = "https://elections-eu.api.ksfmedia.fi/v1"
              , ELECTION_TYPE = "EU"
              }
          }
        , App::{
          , name = "Elections (Parliament)"
          , buildDir = "elections"
          , deployDir = "elections"
          , env = toMap
              { ELECTION_BACKEND_URL = "https://election.api.ksfmedia.fi/v1"
              , ELECTION_TYPE = "PARLIAMENT"
              }
          }
        , App::{
          , name = "App article"
          , buildDir = "app-article"
          , deployDir = "app-article"
          , env = toMap
              { HIDE_LOGIN_LINKS = "true"
              , PRODUCTION_SENTRY_DSN =
                  "https://a28dc2fb33d14a269f87ee4b3f2916d8@o360888.ingest.sentry.io/5707650"
              }
          , lockfile = Some "yarn.lock"
          , caches = Some
              ''
              apps/app-article/dist
              ''
          }
        , App::{
          , name = "Corona banner"
          , buildDir = "corona-banner"
          , deployDir = "corona-banner"
          , lockfile = Some "yarn.lock"
          , caches = Some
              ''
              apps/corona-banner/dist
              ''
          }
        , App::{
          , name = "HBL365"
          , buildDir = "hbl365"
          , deployDir = "hbl365"
          , lockfile = Some "yarn.lock"
          , caches = Some
              ''
              apps/hbl365/.spago
              apps/hbl365/output
              ''
          }
        , App::{
          , name = "Prenumerera"
          , buildDir = "prenumerera"
          , deployDir = "prenumerera"
          }
        , App::{
          , name = "Mosaico"
          , buildDir = "mosaico"
          , deployDir = "mosaico"
          , lockfile = Some "yarn.lock"
          , env = toMap
              { LETTERA_URL = "https://lettera.api.ksfmedia.fi/v4beta"
              }
          , caches = Some
          ''
          apps/mosaico/.spago
          apps/mosaico/output
          apps/mosaico/.cache
          ''
          }
        ]
      : List App.Type

in  { apps, App }
