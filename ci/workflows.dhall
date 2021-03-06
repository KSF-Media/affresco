let Prelude = ./Prelude.dhall

let Map = Prelude.Map.Type

let Env = < Staging | Production >

let App =
      { Type =
          { buildDir : Text
          , deployDir : Text
          , name : Text
          , env : Map Text Text
          }
      , default.env = [] : Map Text Text
      }

let AppServer =
      { Type =
          { id : Text
          , buildDir : Text
          , deployDir : Text
          , name : Text
          , previewUrl : Text
          , runtime : Text
          , entrypoint : Text
          , env : Map Text Text
          }
      , default = { env = [] : Map Text Text, previewUrl = "" : Text }
      }

let Step =
      { Type =
          { id : Optional Text
          , name : Optional Text
          , uses : Optional Text
          , run : Optional Text
          , `with` : Map Text Text
          , env : Map Text Text
          }
      , default =
        { id = None Text
        , name = None Text
        , uses = None Text
        , run = None Text
        , `with` = [] : Map Text Text
        , env = [] : Map Text Text
        }
      }

let setupSteps =
      [ Step::{ name = Some "Checkout repo", uses = Some "actions/checkout@v2" }
      , Step::{
        , name = Some "Setup node and yarn"
        , uses = Some "actions/setup-node@v1"
        , `with` = toMap { node-version = "12" }
        }
      , Step::{
        , name = Some "Setup ruby"
        , uses = Some "actions/setup-ruby@v1"
        , `with` = toMap { ruby-version = "2.6" }
        }
      , Step::{
        , uses = Some "cachix/install-nix-action@v12"
        , `with` = toMap { nix_path = "nixpkgs=channel:nixos-20.09" }
        }
      , Step::{
        , run = Some
            ''
              yarn install --pure-lockfile
              mkdir -p build
            ''
        }
      ]

let mkBuildStep =
      \(app : App.Type) ->
        Step::{
        , name = Some "Build ${app.name}"
        , env = app.env
        , run = Some
            ''
              ruby deploy.rb ${app.buildDir}
              mv apps/${app.buildDir}/dist build/${app.deployDir}
            ''
        }

let mkBuildServerStep =
      \(app : AppServer.Type) ->
        Step::{
        , name = Some "Build Server ${app.name}"
        , env = app.env
        , run = Some
            ''
              ruby deploy.rb ${app.buildDir}
              mv apps/${app.buildDir} build/${app.deployDir}
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
                  { Staging = "deploy-previews/\${{ github.sha }}"
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
      \(app : AppServer.Type) ->
        Step::{
        , id = Some "deploy-${app.id}"
        , name = Some "Deploy ${app.name}"
        , uses = Some "google-github-actions/deploy-appengine@main"
        , `with` = toMap
            { working_directory = "build/${app.deployDir}"
            , promote = "true"
            , project_id =
                merge
                  { Staging = "\${{ secrets.GCP_STAGING_PROJECT_ID }}"
                  , Production = "\${{ secrets.GCP_PRODUCTION_PROJECT_ID }}"
                  }
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
            { working_directory = "build"
            , deliverables = "dispatch.yaml"
            , project_id =
                merge
                  { Staging = "\${{ secrets.GCP_STAGING_PROJECT_ID }}"
                  , Production = "\${{ secrets.GCP_PRODUCTION_PROJECT_ID }}"
                  }
                  env
            , credentials =
                merge
                  { Staging = "\${{ secrets.GCP_STAGING_AE_KEY }}"
                  , Production = "\${{ secrets.GCP_PRODUCTION_AE_KEY }}"
                  }
                  env
            }
        }

let checkCIStep =
      Step::{
      , name = Some "Check CI script has been generated from Dhall"
      , run = Some
          ''
            make
            git diff --exit-code
          ''
      }

let generateDispatchYamlStep =
      \(env : Env) ->
        Step::{
        , name = Some "Generate AppEngine domain map"
        , run =
            merge
              { Staging = Some
                  ''
                    nix-shell ci/dhall.nix --run 'dhall-to-yaml --omit-empty \
                    <<< "./ci/dispatch.yaml.dhall" <<< "<Staging|Production>.Staging"' > ./build/dispatch.yaml
                  ''
              , Production = Some
                  ''
                    nix-shell ci/dhall.nix --run 'dhall-to-yaml --omit-empty \
                    <<< "./ci/dispatch.yaml.dhall" <<< "<Staging|Production>.Production"' > ./build/dispatch.yaml
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
        , uses = Some "unsplash/comment-on-pr@master"
        , env = toMap { GITHUB_TOKEN = "\${{ secrets.GITHUB_TOKEN }}" }
        , `with` = toMap
            { msg =
                let renderAppLink =
                      \(app : App.Type) ->
                        "- [${app.name}](${previewUrl}/${app.deployDir}/index.html)"

                let renderAELink =
                      \(app : AppServer.Type) ->
                        "- [${app.name}](\${{ steps.deploy-${app.id}.outputs.url }}/${app.previewUrl})"

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
            }
        }

let refreshCDNSteps =
      \(cdnName : Text) ->
        [ Step::{
          , name = Some "Install gcloud"
          , uses = Some "GoogleCloudPlatform/github-actions/setup-gcloud@master"
          , `with` = toMap
              { project_id = "\${{ secrets.GCP_PRODUCTION_PROJECT_ID }}"
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
        { runs-on = "ubuntu-latest"
        , steps = refreshCDNSteps cdnName
        , needs = "deploy"
        }

let uploadSteps =
      \(env : Env) -> Prelude.List.map App.Type Step.Type (mkUploadStep env)

let deployAppEngineSteps =
      \(env : Env) ->
        Prelude.List.map AppServer.Type Step.Type (mkAppEngineStep env)

let buildSteps = Prelude.List.map App.Type Step.Type mkBuildStep

let buildServerSteps =
      Prelude.List.map AppServer.Type Step.Type mkBuildServerStep

in  { Step
    , Prelude
    , App
    , AppServer
    , Env
    , setupSteps
    , buildSteps
    , buildServerSteps
    , uploadSteps
    , deployAppEngineSteps
    , checkCIStep
    , linkPreviewsStep
    , refreshCDNJob
    , generateDispatchYamlStep
    , deployDispatchYamlStep
    }
