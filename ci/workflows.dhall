let Prelude = ./Prelude.dhall

let Map = Prelude.Map.Type

let default = Prelude.Text.default

let not = Prelude.Bool.not

let null = Prelude.Optional.null

let Env = < Staging | Production >

let App =
      { Type =
          { buildDir : Text
          , deployDir : Text
          , name : Text
          , env : Map Text Text
          , lockfile : Optional Text
          , caches : Optional Text
          }
      , default =
        { env = [] : Map Text Text, lockfile = None Text, caches = None Text }
      }

let AppServer =
      { Type =
          { id : Text
          , buildDir : Text
          , deployDir : Text
          , name : Text
          , runtime : Text
          , entrypoint : Text
          , env : Map Text Text
          , previewUrl : Text
          , lockfile : Optional Text
          , caches : Optional Text
          , domains : List Text
          }
      , default =
        { env = [] : Map Text Text
        , previewUrl = ""
        , lockfile = None Text
        , caches = None Text
        , domains = [] : List Text
        }
      }

let Step =
      { Type =
          { id : Optional Text
          , name : Optional Text
          , uses : Optional Text
          , run : Optional Text
          , `with` : Map Text Text
          , env : Map Text Text
          , shell : Optional Text
          , continue-on-error : Optional Bool
          }
      , default =
        { id = None Text
        , name = None Text
        , uses = None Text
        , run = None Text
        , `with` = [] : Map Text Text
        , env = [] : Map Text Text
        , shell = None Text
        , continue-on-error = None Bool
        }
      }

let setupSteps =
      \(env : Env) ->
        [ Step::{
          , name = Some "Checkout repo"
          , uses = Some "actions/checkout@v2"
          }
        , Step::{
          , name = Some "Setup Cloud SDK"
          , uses = Some "google-github-actions/setup-gcloud@master"
          , `with` = toMap
              { project_id =
                  merge
                    { Staging = "ksf-staging", Production = "ksf-production" }
                    env
              , service_account_key =
                  merge
                    { Staging = "\${{ secrets.GCP_STAGING_AE_KEY }}"
                    , Production = "\${{ secrets.GCP_PRODUCTION_AE_KEY }}"
                    }
                    env
              , export_default_credentials = "true"
              }
          }
        , Step::{
          , run = Some
              ''
                yarn install --pure-lockfile --cache-folder=.yarn-cache
                mkdir -p build
              ''
          }
        ]

let mkCacheAppStep =
      \(app : App.Type) ->
        let caches = default app.caches

        let lockfile = default app.lockfile

        in  Step::{
            , name = Some "Setup build cache for ${app.name}"
            , uses = Some "actions/cache@v2"
            , `with` = toMap
                { path = caches
                , key =
                    "\${{ runner.os }}-${app.deployDir}-\${{ hashFiles('apps/${app.buildDir}/${lockfile}')}}"
                }
            }

let mkBuildStep =
      \(app : App.Type) ->
        Step::{
        , name = Some "Build ${app.name}"
        , env = app.env
        , shell = Some "bash"
        , run = Some
            ''
              ruby deploy.rb ${app.buildDir}
              cp -R apps/${app.buildDir}/dist build/${app.deployDir}
            ''
        }

let mkBuildServerStep =
      \(app : AppServer.Type) ->
        Step::{
        , name = Some "Build Server ${app.name}"
        , env = app.env
        , shell = Some "bash"
        , run = Some
            ''
              ruby deploy.rb ${app.buildDir}
              cp -R apps/${app.buildDir} build/${app.deployDir}
            ''
        }

let mkUploadStep =
      \(env : Env) ->
      \(app : App.Type) ->
        Step::{
        , name = Some "Upload ${app.name}"
        , uses = Some "google-github-actions/upload-cloud-storage@main"
        , `with` = toMap
            { path = "build/${app.deployDir}"
            , destination =
                merge
                  { Staging =
                      "deploy-previews/\${{ github.sha }}/${app.deployDir}"
                  , Production = "ksf-frontends/${app.deployDir}"
                  }
                  env
            , parent = "false"
            , credentials =
                merge
                  { Staging = "\${{ secrets.GCP_PREVIEW_KEY }}"
                  , Production = "\${{ secrets.GCP_PRODUCTION_KEY }}"
                  }
                  env
            }
        }

let mkAppEngineStep =
      \(env : Env) ->
      \(promote : Text) ->
      \(app : AppServer.Type) ->
        Step::{
        , id =
            merge
              { Staging = Some "deploy-${app.id}"
              , Production = Some "deploy-${app.id}-production"
              }
              env
        , name = Some "Deploy ${app.name}"
        , uses = Some "google-github-actions/deploy-appengine@main"
        , `with` = toMap
            { working_directory = "build/${app.deployDir}"
            , promote
            , project_id =
                merge
                  { Staging = "ksf-staging", Production = "ksf-production" }
                  env
            , credentials =
                merge
                  { Staging = "\${{ secrets.GCP_STAGING_AE_KEY }}"
                  , Production = "\${{ secrets.GCP_PRODUCTION_AE_KEY }}"
                  }
                  env
            }
        }

let deployDispatchYamlStep =
      \(env : Env) ->
        Step::{
        , name = Some "Deploy AppEngine domain map"
        , uses = Some "google-github-actions/deploy-appengine@main"
        , `with` = toMap
            { deliverables = "dispatch.yaml"
            , project_id =
                merge
                  { Staging = "ksf-staging", Production = "ksf-production" }
                  env
            , credentials =
                merge
                  { Staging = "\${{ secrets.GCP_STAGING_AE_KEY }}"
                  , Production = "\${{ secrets.GCP_PRODUCTION_AE_KEY }}"
                  }
                  env
            }
        }

let checkCISteps =
      [ Step::{ name = Some "Checkout repo", uses = Some "actions/checkout@v2" }
      , Step::{
        , name = Some "Check CI script has been generated from Dhall"
        , run = Some
            ''
              make
              git diff --exit-code
            ''
        }
      ]

let generateDispatchYamlStep =
      \(env : Env) ->
        Step::{
        , name = Some "Generate AppEngine domain map"
        , shell = Some "bash"
        , run =
            merge
              { Staging = Some
                  ''
                    npx 'dhall-to-yaml --omit-empty \
                    <<< "./ci/dispatch.yaml.dhall" <<< "<Staging|Production>.Staging"' > ./dispatch.yaml
                  ''
              , Production = Some
                  ''
                    npx 'dhall-to-yaml --omit-empty \
                    <<< "./ci/dispatch.yaml.dhall" <<< "<Staging|Production>.Production"' > ./dispatch.yaml
                  ''
              }
              env
        }

let linkPreviewsStep =
      \(apps : List App.Type) ->
      \(appServers : List AppServer.Type) ->
      \(previewUrl : Text) ->
        Step::{
        , name = Some "Post preview links"
        , uses = Some
            "unsplash/comment-on-pr@ffe8f97ccc63ce12c3c23c6885b169db67958d3b"
        , env = toMap { GITHUB_TOKEN = "\${{ secrets.GITHUB_TOKEN }}" }
        , `with` = toMap
            { msg =
                let renderAppLink =
                      \(app : App.Type) ->
                        "- [${app.name}](${previewUrl}/${app.deployDir}/index.html)"

                let renderAELink =
                      \(app : AppServer.Type) ->
                        "- [${app.name}](\${{ needs.deploy-${app.id}.outputs.preview }}/${app.previewUrl})"

                in  ''
                    Deploy previews are ready :sunglasses:
                    ${Prelude.Text.concatMapSep
                        "\n"
                        App.Type
                        renderAppLink
                        apps}
                    ${Prelude.Text.concatMapSep
                        "\n"
                        AppServer.Type
                        renderAELink
                        appServers}
                    ''
            , check_for_duplicate_msg = "false"
            }
        }

let mkCleanAppEngineStep =
      \(env : Env) ->
      \(app : AppServer.Type) ->
        Step::{
        , name = Some "Keep only 10 latest versions of ${app.id}"
        , continue-on-error = Some True
        , run = Some
            ''
            ./ci/ae-cleanup.sh ${app.id}
            ''
        }

let refreshCDNSteps =
      \(cdnName : Text) ->
        [ Step::{
          , name = Some "Install gcloud"
          , uses = Some "google-github-actions/setup-gcloud@master"
          , `with` = toMap
              { project_id = "ksf-production"
              , service_account_key = "\${{ secrets.GCP_PRODUCTION_KEY }}"
              , export_default_credentials = "true"
              }
          }
        , Step::{
          , name = Some "Invalidate CDN cache for '${cdnName}'"
          , run = Some
              ''
                gcloud compute url-maps invalidate-cdn-cache ${cdnName} --path "/*"
              ''
          }
        ]

let refreshCDNJob =
      \(cdnName : Text) ->
      \(jobName : Text) ->
        { runs-on = "ubuntu-latest"
        , steps = refreshCDNSteps cdnName
        , needs = jobName
        }

let uploadSteps =
      \(env : Env) -> Prelude.List.map App.Type Step.Type (mkUploadStep env)

let deployAppEngineSteps =
      \(env : Env) ->
      \(promote : Text) ->
        Prelude.List.map AppServer.Type Step.Type (mkAppEngineStep env promote)

let buildSteps = Prelude.List.map App.Type Step.Type mkBuildStep

let buildServerSteps =
      Prelude.List.map AppServer.Type Step.Type mkBuildServerStep

let cleanAppEngineSteps =
      \(env : Env) ->
        Prelude.List.map AppServer.Type Step.Type (mkCleanAppEngineStep env)

let cacheSteps = Prelude.List.map App.Type Step.Type mkCacheAppStep

let hasLockfile
    : App.Type -> Bool
    = \(a : App.Type) -> not (null Text a.lockfile)

in  { Step
    , Prelude
    , App
    , AppServer
    , Env
    , setupSteps
    , buildSteps
    , buildServerSteps
    , mkBuildServerStep
    , uploadSteps
    , deployAppEngineSteps
    , mkAppEngineStep
    , checkCISteps
    , linkPreviewsStep
    , refreshCDNJob
    , generateDispatchYamlStep
    , deployDispatchYamlStep
    , cacheSteps
    , hasLockfile
    , cleanAppEngineSteps
    , mkCleanAppEngineStep
    }
