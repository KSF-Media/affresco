{-

Template to generate the `.github/workflows/previews.yml` CI file

-}
let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let apps = ./apps.dhall

let AE = ./app-servers.dhall

let previewUrl = "https://deploy-previews.ksfmedia.fi/\${{ github.sha }}"

let container = ./container.dhall

let gcp-project-id = "ksf-staging"

let apps-to-cache =
      Prelude.List.filter Actions.App.Type Actions.hasLockfile apps

let checkCISteps = Actions.checkCISteps

let steps-gs =
        Actions.setupSteps Actions.Env.Staging
      # Actions.cacheSteps apps-to-cache
      # Actions.buildSteps apps
      # Actions.uploadSteps Actions.Env.Staging apps

let steps-app-article =
        Actions.setupSteps Actions.Env.Staging
      # [ Actions.mkBuildServerStep AE.servers.app-article-server ]
      # [ Actions.mkAppEngineStep
            Actions.Env.Staging
            AE.servers.app-article-server
        ]
      # [ Actions.mkCleanAppEngineStep
            Actions.Env.Staging
            AE.servers.app-article-server
        ]

let steps-mosaico =
        Actions.setupSteps Actions.Env.Staging
      # [ Actions.mkBuildServerStep AE.servers.mosaico ]
      # [ Actions.mkAppEngineStep Actions.Env.Staging AE.servers.mosaico ]
      # [ Actions.mkCleanAppEngineStep Actions.Env.Staging AE.servers.mosaico ]

let previewLinks = [ Actions.linkPreviewsStep apps AE.all previewUrl ]

in  { name = "previews"
    , on.pull_request.branches = [ "master" ]
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
        , outputs.preview = "\${{ steps.deploy-app-article-server.outputs.url}}"
        , needs = "check-ci"
        }
      , deploy-mosaico =
        { runs-on = "ubuntu-latest"
        , env.gcp-project-id = gcp-project-id
        , container
        , steps = steps-mosaico
        , outputs.preview = "\${{ steps.deploy-mosaico-server.outputs.url}}"
        , needs = "check-ci"
        }
      , previews =
        { runs-on = "ubuntu-latest"
        , steps = previewLinks
        , needs = [ "deploy-gs", "deploy-mosaico", "deploy-app-article" ]
        }
      }
    }
