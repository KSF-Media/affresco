let Prelude = ./Prelude.dhall

let A = ./apps.dhall

let Map = Prelude.Map.Type

let default = Prelude.Text.default

let not = Prelude.Bool.not

let null = Prelude.Optional.null

let Env = < Staging | Production >

let App = A.App

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
          , uses = Some "actions/checkout@v4"
          }
        , Step::{
          , name = Some "Auth Cloud SDK"
          , uses = Some "google-github-actions/auth@v2"
          , `with` = toMap
              { project_id =
                  merge
                    { Staging = "ksf-staging", Production = "ksf-production" }
                    env
              , credentials_json =
                  merge
                    { Staging = "\${{ secrets.GCP_PREVIEW_KEY }}"
                    , Production = "\${{ secrets.GCP_PRODUCTION_KEY }}"
                    }
                    env
              , create_credentials_file = "true"
              }
          }
        , Step::{
          , name = Some "Setup Cloud SDK"
          , uses = Some "google-github-actions/setup-gcloud@v2"
          }
        , Step::{
          , run = Some
              ''
                yarn install --pure-lockfile
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
            , uses = Some "actions/cache@v4"
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

let mkUploadStep =
      \(env : Env) ->
      \(app : App.Type) ->
        Step::{
        , name = Some "Upload ${app.name}"
        , uses = Some "google-github-actions/upload-cloud-storage@v2"
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
            }
        }

let checkCISteps =
      [ Step::{ name = Some "Checkout repo", uses = Some "actions/checkout@v4" }
      , Step::{
        , name = Some "Check CI script has been generated from Dhall"
        , run = Some
            ''
              git config --global --add safe.directory $(pwd)
              chmod 666 .github/workflows/*
              make
              chmod 644 .github/workflows/*
              git diff --exit-code
            ''
        }
      ]

let linkPreviewsStep =
      \(apps : List App.Type) ->
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

                in  ''
                    Deploy previews are ready :sunglasses:
                    ${Prelude.Text.concatMapSep
                        "\n"
                        App.Type
                        renderAppLink
                        apps}
                    ''
            , check_for_duplicate_msg = "false"
            }
        }

let refreshCDNSteps =
      \(cdnName : Text) ->
        [ Step::{
          , name = Some "Auth Cloud SDK"
          , uses = Some "google-github-actions/auth@v2"
          , `with` = toMap
              { project_id = "ksf-staging"
              , credentials_json = "\${{ secrets.GCP_PRODUCTION_KEY }}"
              , create_credentials_file = "true"
              }
          }
        , Step::{
          , name = Some "Install gcloud"
          , uses = Some "google-github-actions/setup-gcloud@v2"
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

let buildSteps = Prelude.List.map App.Type Step.Type mkBuildStep

let cacheSteps = Prelude.List.map App.Type Step.Type mkCacheAppStep

let hasLockfile
    : App.Type -> Bool
    = \(a : App.Type) -> not (null Text a.lockfile)

in  { Step
    , Prelude
    , Env
    , setupSteps
    , buildSteps
    , uploadSteps
    , checkCISteps
    , linkPreviewsStep
    , refreshCDNJob
    , cacheSteps
    , hasLockfile
    }
