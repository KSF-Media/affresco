{-

Template to generate the `.github/workflows/previews.yml` CI file

-}
let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let A = ./apps.dhall

let AE = ./app-servers.dhall

let AS = ./app-servers/AppServer.dhall

let AppServer = AS.AppServer

let container = ./container.dhall

let App = A.App

let apps = A.apps

let previewUrl = "https://deploy-previews.ksfmedia.fi/\${{ github.sha }}"

let promote = "false"

let apps-to-cache = Prelude.List.filter App.Type Actions.hasLockfile apps

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
            promote
            AE.servers.app-article-server
        ]
      # [ Actions.mkCleanAppEngineStep
            Actions.Env.Staging
            AE.servers.app-article-server
        ]

let steps-mosaico =
        Actions.setupSteps Actions.Env.Staging
      # [ Actions.mkBuildServerStep AE.servers.mosaico ]
      # [ Actions.mkAppEngineStep Actions.Env.Staging promote AE.servers.mosaico
        ]
      # [ Actions.mkCleanAppEngineStep Actions.Env.Staging AE.servers.mosaico ]

let steps-dispatch =
        Actions.setupSteps Actions.Env.Staging
      # [ Actions.generateDispatchYamlStep Actions.Env.Staging ]
      # [ Actions.deployDispatchYamlStep Actions.Env.Staging ]

let previewLinks = [ Actions.linkPreviewsStep apps AE.all previewUrl ]

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
      , deploy-app-article-server =
        { runs-on = "ubuntu-latest"
        , container
        , steps = steps-app-article
        , outputs.preview = "\${{ steps.deploy-app-article-server.outputs.url}}"
        , needs = "check-ci"
        }
      , deploy-mosaico-server =
        { runs-on = "ubuntu-latest"
        , container
        , steps = steps-mosaico
        , outputs.preview = "\${{ steps.deploy-mosaico-server.outputs.url}}"
        , needs = "check-ci"
        }
      , deploy-dispatch-yaml =
        { runs-on = "ubuntu-latest"
        , container
        , steps = steps-dispatch
        , needs = [ "deploy-mosaico-server", "deploy-app-article-server" ]
        }
      , previews =
        { runs-on = "ubuntu-latest"
        , steps = previewLinks
        , needs =
          [ "deploy-gs", "deploy-mosaico-server", "deploy-app-article-server" ]
        }
      }
    }
