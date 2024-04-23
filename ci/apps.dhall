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
          , production : Bool
          }
      , default =
        { env = [] : Map Text Text
        , lockfile = None Text
        , caches = None Text
        , production = True
        }
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
          , name = "Kort"
          , buildDir = "kort"
          , deployDir = "kort"
          , env = toMap
              { PRODUCTION_SENTRY_DSN =
                  "https://54e59357e2fd42db917041739865e2c9@sentry.io/5174203"
              , MAINTENANCE_MODE = "false"
              }
          , lockfile = Some "yarn.lock"
          , caches = Some
              ''
              apps/kort/.spago
              apps/kort/output
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
          , name = "Prenumerera"
          , buildDir = "prenumerera"
          , deployDir = "prenumerera"
          , env = toMap {
            MAINTENANCE_MODE = "false"
          }
          }
        ]
      : List App.Type

in  { apps, App }
