{-

Template to generate the `.github/workflows/production.yml` CI file

-}
let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let A = ./apps.dhall

let App = A.App

let apps = Prelude.List.filter App.Type (\(a: App.Type) -> a.production)  A.apps

let container = ./container.dhall

let promote = "true"

let apps-to-cache = Prelude.List.filter App.Type Actions.hasLockfile apps

let checkCISteps = Actions.checkCISteps

let steps-gs =
        Actions.setupSteps Actions.Env.Production
      # Actions.cacheSteps apps-to-cache
      # Actions.buildSteps apps
      # Actions.uploadSteps Actions.Env.Production apps

let refreshCDNJobs =
      { refresh_cdn_mitt-konto = Actions.refreshCDNJob "mitt-konto" "deploy-gs"
      , refresh_cdn_kort = Actions.refreshCDNJob "kort" "deploy-gs"
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
            }
        //  refreshCDNJobs
    , env =
      { NODE_ENV = "production"
      , PRODUCTION_BOTTEGA_URL = "https://bottega.api.ksfmedia.fi/v1"
      , PRODUCTION_PERSONA_URL = "https://persona.api.ksfmedia.fi/v1"
      , PRODUCTION_LETTERA_URL = "https://lettera.api.ksfmedia.fi/v3"
      , PRODUCTION_DUELLEN_URL = "https://duellen.api.ksfmedia.fi"
      }
    }
