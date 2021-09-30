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

let steps =
        Actions.setupSteps
      # [ Actions.checkCIStep ]
      # Actions.cacheSteps apps-to-cache
      # Actions.buildSteps apps
      # Actions.buildServerSteps app-servers
      # Actions.uploadSteps Actions.Env.Staging apps
      # Actions.deployAppEngineSteps Actions.Env.Staging app-servers
      # [ Actions.linkPreviewsStep apps app-servers previewUrl ]
      # Actions.cleanAppEngineSteps Actions.Env.Staging app-servers

in  { name = "previews"
    , on.pull_request.branches = [ "master" ]
    , jobs.deploy = { runs-on = "ubuntu-latest", steps }
    }
