{-

Template to generate the `.github/workflows/previews.yml` CI file

-}
let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let apps = ./apps.dhall

let app-servers = ./app-servers.dhall

let previewUrl = "https://deploy-previews.ksfmedia.fi/\${{ github.sha }}"

let apps-to-cache =
      Prelude.List.filter Actions.App.Type Actions.hasLockfile apps

let checkCISteps = Actions.checkCISteps

let steps-gs =
        Actions.setupSteps Actions.Env.Staging
      # Actions.cacheSteps apps-to-cache
      # Actions.buildSteps apps
      # Actions.uploadSteps Actions.Env.Staging apps

let steps-ae =
        Actions.setupSteps Actions.Env.Staging
      # Actions.buildServerSteps app-servers
      # Actions.deployAppEngineSteps Actions.Env.Staging app-servers
      # Actions.cleanAppEngineSteps Actions.Env.Staging app-servers

let previewLinks = [ Actions.linkPreviewsStep apps app-servers previewUrl ]

in  { name = "previews"
    , on.pull_request.branches = [ "master" ]
    , jobs =
      { check-ci = { runs-on = "ubuntu-latest", steps = checkCISteps }
      , deploy-gs =
        { runs-on = "ubuntu-latest", steps = steps-gs, needs = "check-ci" }
      , deploy-ae =
        { runs-on = "ubuntu-latest", steps = steps-ae, needs = "check-ci" }
      , previews =
        { runs-on = "ubuntu-latest"
        , steps = previewLinks
        , needs = [ "deploy-gs", "deploy-ae" ]
        }
      }
    }
