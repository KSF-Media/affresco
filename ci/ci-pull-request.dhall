{-

Template to generate the `.github/workflows/previews.yml` CI file

-}
let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let A = ./apps.dhall

let container = ./container.dhall

let App = A.App

let apps = A.apps

let previewUrl = "https://deploy-previews.ksfmedia.fi/\${{ github.sha }}"

let apps-to-cache = Prelude.List.filter App.Type Actions.hasLockfile apps

let checkCISteps = Actions.checkCISteps

let steps-gs =
        Actions.setupSteps Actions.Env.Staging
      # Actions.cacheSteps apps-to-cache
      # Actions.buildSteps apps
      # Actions.uploadSteps Actions.Env.Staging apps

let previewLinks =
      [ Actions.linkPreviewsStep
          apps
          previewUrl
      ]

in  { name = "previews"
    , on.pull_request.branches = [ "master" ]
    , jobs =
      { check-ci =
        { runs-on = "ubuntu-latest", container, steps = checkCISteps }
      , deploy-gs =
        { runs-on = "ubuntu-latest"
        , container
        , steps = steps-gs
        , needs = "check-ci"
        }
      , previews =
        { runs-on = "ubuntu-latest"
        , steps = previewLinks
        , needs =
          [ "deploy-gs"]
        }
      }
    , env =
      { NODE_ENV = "production"
      }
    }
