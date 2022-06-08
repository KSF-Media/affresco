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

let mkAeSteps =
      \(env : Actions.Env) ->
      \(app : AppServer.Type) ->
          Actions.setupSteps env
        # [ Actions.mkBuildServerStep app ]
        # [ Actions.copyAppYamlForStaging app ]
        # [ Actions.mkAppEngineStep env promote app ]
        # [ Actions.mkCleanAppEngineStep env app ]

let steps-gs =
        Actions.setupSteps Actions.Env.Staging
      # Actions.cacheSteps apps-to-cache
      # Actions.buildSteps apps
      # Actions.uploadSteps Actions.Env.Staging apps

let steps-app-article = mkAeSteps Actions.Env.Staging AE.app-article-server

let steps-dispatch =
        Actions.setupSteps Actions.Env.Staging
      # [ Actions.generateDispatchYamlStep Actions.Env.Staging ]
      # [ Actions.deployDispatchYamlStep Actions.Env.Staging ]

let previewLinks =
      [ Actions.linkPreviewsStep
          apps
          (Prelude.Map.values Text AppServer.Type (toMap AE))
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
      , deploy-app-article-server =
        { runs-on = "ubuntu-latest"
        , container
        , steps = steps-app-article
        , outputs.preview = "\${{ steps.deploy-app-article-server.outputs.url}}"
        , needs = "check-ci"
        }
      , previews =
        { runs-on = "ubuntu-latest"
        , steps = previewLinks
        , needs =
          [ "deploy-gs", "deploy-app-article-server" ]
        }
      }
    , env =
      { NODE_ENV = "production"
      , OPENSSL_CONF = "/dev/null" -- remove when upgrading to node 18
      }
    }
