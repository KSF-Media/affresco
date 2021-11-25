{-

Template to generate the `.github/workflows/production.yml` CI file

-}
let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let A = ./apps.dhall

let apps = A.apps

let App = A.App

let AE = ./app-servers.dhall

let AS = ./app-servers/AppServer.dhall

let AppServer = AS.AppServer

let container = ./container.dhall

let promote = "true"

let apps-to-cache = Prelude.List.filter App.Type Actions.hasLockfile apps

let checkCISteps = Actions.checkCISteps

let mkAeSteps =
      \(env : Actions.Env) ->
      \(app : AppServer.Type) ->
          Actions.setupSteps env
        # [ Actions.mkBuildServerStep app ]
        # [ Actions.generateAppYaml app ]
        # [ Actions.mkAppEngineStep env promote app ]
        # [ Actions.copyAppYamlForStaging app ]
        # [ Actions.mkAppEngineStep Actions.Env.Staging promote app ]
        # [ Actions.mkCleanAppEngineStep env app ]

let steps-gs =
        Actions.setupSteps Actions.Env.Production
      # Actions.cacheSteps apps-to-cache
      # Actions.buildSteps apps
      # Actions.uploadSteps Actions.Env.Production apps

let steps-app-article = mkAeSteps Actions.Env.Production AE.app-article-server

let steps-mosaico = mkAeSteps Actions.Env.Production AE.mosaico-server

let steps-dispatch =
        Actions.setupSteps Actions.Env.Production
      # [ Actions.generateDispatchYamlStep Actions.Env.Production ]
      # [ Actions.deployDispatchYamlStep Actions.Env.Production ]

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
              , container
              , steps = steps-gs
              , needs = "check-ci"
              }
            , deploy-app-article-server =
              { runs-on = "ubuntu-latest"
              , container
              , steps = steps-app-article
              , needs = "check-ci"
              }
            , deploy-mosaico-server =
              { runs-on = "ubuntu-latest"
              , container
              , steps = steps-mosaico
              , needs = "check-ci"
              }
            , deploy-dispatch-yaml =
              { runs-on = "ubuntu-latest"
              , container
              , steps = steps-dispatch
              , needs = [ "deploy-mosaico-server", "deploy-app-article-server" ]
              }
            }
        //  refreshCDNJobs
    , env =
      { PRODUCTION_FACEBOOK_APP_ID = "894000011480431"
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
