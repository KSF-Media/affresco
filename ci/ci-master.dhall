{-

Template to generate the `.github/workflows/production.yml` CI file

-}
let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let apps = ./apps.dhall

let AE = ./app-servers.dhall

let container = ./container.dhall

let gcp-project-id = "ksf-production"

let promote = "true"

let apps-to-cache =
      Prelude.List.filter Actions.App.Type Actions.hasLockfile apps

let checkCISteps = Actions.checkCISteps

let steps-gs =
        Actions.setupSteps Actions.Env.Production
      # Actions.cacheSteps apps-to-cache
      # Actions.buildSteps apps
      # Actions.uploadSteps Actions.Env.Production apps

let steps-app-article =
        Actions.setupSteps Actions.Env.Production
      # [ Actions.mkBuildServerStep AE.servers.app-article-server ]
      # [ Actions.mkAppEngineStep
            Actions.Env.Production
            promote
            AE.servers.app-article-server
        ]
      # [ Actions.mkCleanAppEngineStep
            Actions.Env.Production
            AE.servers.app-article-server
        ]

let steps-mosaico =
        Actions.setupSteps Actions.Env.Production
      # [ Actions.mkBuildServerStep AE.servers.mosaico ]
      # [ Actions.mkAppEngineStep Actions.Env.Staging promote AE.servers.mosaico
        ]
      # [ Actions.mkAppEngineStep
            Actions.Env.Production
            promote
            AE.servers.mosaico
        ]
      # [ Actions.mkCleanAppEngineStep Actions.Env.Production AE.servers.mosaico
        ]

let refreshCDNJobs =
      { refresh_cdn_mitt-konto = Actions.refreshCDNJob "mitt-konto" "deploy-gs"
      , refresh_cdn_app-article =
          Actions.refreshCDNJob "app-article" "deploy-gs"
      , refresh_cdn_frontends =
          Actions.refreshCDNJob "ksf-frontends-lb" "deploy-gs"
      }

in  { name = "production"
    , on.push.branches = [ "master" ]
    , jobs =
            { check-ci =
              { runs-on = "ubuntu-latest", container, steps = checkCISteps }
            , deploy-gs =
              { runs-on = "ubuntu-latest"
              , env.gcp-project-id = gcp-project-id
              , container
              , steps = steps-gs
              , needs = "check-ci"
              }
            , deploy-app-article =
              { runs-on = "ubuntu-latest"
              , env.gcp-project-id = gcp-project-id
              , container
              , steps = steps-app-article
              , needs = "check-ci"
              }
            , deploy-mosaico =
              { runs-on = "ubuntu-latest"
              , env.gcp-project-id = gcp-project-id
              , container
              , steps = steps-mosaico
              , needs = "check-ci"
              }
            }
        //  refreshCDNJobs
    , env =
      { PRODUCTION_FACEBOOK_APP_ID = "1742585382627694"
      , PRODUCTION_GOOGLE_CLIENT_ID =
          "584250859572-po558qgkq0b4u3j6a1ge40scjhops3oo"
      , PRODUCTION_JANRAIN_FLOW_VERSION = "20201223120649370442"
      , PRODUCTION_JANRAIN_LOGIN_CLIENT_ID = "jzbqvf3p68e8hje6wrzhezk98m8qndmz"
      , PRODUCTION_JANRAIN_SSO_SERVER = "https://ksf-media.eu.janrainsso.com"
      , PRODUCTION_JANRAIN_XD_RECEIVER_PATH = "/xd_receiver.html"
      , PRODUCTION_BOTTEGA_URL = "https://bottega.api.ksfmedia.fi/v1"
      , PRODUCTION_PERSONA_URL = "https://persona.api.ksfmedia.fi/v1"
      , PRODUCTION_LETTERA_URL = "https://lettera.api.ksfmedia.fi/v3"
      , PRODUCTION_DUELLEN_URL = "https://duellen.api.ksfmedia.fi"
      }
    }
