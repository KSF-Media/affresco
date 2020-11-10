let Prelude = ./Prelude.dhall

let Map = Prelude.Map.Type

let Env = < Staging | Production >

let App =
  { Type = {
    -- The directory name in the `apps` folder of this repo
    , buildDir : Text
    -- The directory of the bucket where the app will be deployed
    , deployDir : Text
    -- The "friendly" name of the app (e.g. can contain spaces)
    , name : Text
    -- Environment variables to be supplied in production (in addition to the global ones)
    , env : Map Text Text
    }
  , default = { env = [] : Map Text Text }
  }

let Step =
  { Type = {
    , name : Optional Text
    , uses : Optional Text
    , run : Optional Text
    , `with` : Map Text Text
    , env : Map Text Text
    }
  , default = {
    , name = None Text
    , uses = None Text
    , run = None Text
    , `with` = [] : Map Text Text
    , env = [] : Map Text Text
    }
  }

let setupSteps = [
  , Step::{
    , name = Some "Checkout repo"
    , uses = Some "actions/checkout@v2"
  }
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
  , Step::{ uses = Some "cachix/install-nix-action@v9" }
  , Step::{
    , run = Some ''
      yarn install --pure-lockfile
      mkdir -p build
    ''
  }
]

let mkBuildStep
  = \(app : App.Type)
  -> Step::{
    , name = Some "Build ${app.name}"
    , env = app.env
    , run = Some ''
      ruby deploy.rb ${app.buildDir}
      mv apps/${app.buildDir}/dist build/${app.deployDir}
    ''
  }

let mkUploadStep
  = \(env : Env)
  -> \(app : App.Type)
  -> Step::{
    , name = Some "Upload ${app.name}"
    , uses = Some "GoogleCloudPlatform/github-actions/upload-cloud-storage@master"
    , `with` = toMap {
      , path = "build/${app.deployDir}"
      , destination =
          merge
            { Staging = "deploy-previews/\${{ github.sha }}"
            , Production = "ksf-frontends"
            }
            env
      , credentials =
          merge
            { Staging = "\${{ secrets.GCP_PREVIEW_KEY }}"
            , Production = "\${{ secrets.GCP_PRODUCTION_KEY }}"
            }
            env
    }
  }

let checkCIStep
  = Step::{
  , name = Some "Check CI script has been generated from Dhall"
  , run = Some ''
      make
      git diff --exit-code
    ''
  }

let linkPreviewsStep
  = \(apps : List App.Type)
  -> \(previewUrl : Text)
  -> Step::{
  , name = Some "Post preview links"
  , uses = Some "unsplash/comment-on-pr@master"
  , env = toMap { GITHUB_TOKEN = "\${{ secrets.GITHUB_TOKEN }}" }
  , `with` = toMap {
    , msg =
      let renderAppLink = \(app : App.Type) -> "- [${app.name}](${previewUrl}/${app.deployDir}/index.html)"
      in ''
      Deploy previews are ready :sunglasses:
      ${Prelude.Text.concatMapSep "\n" App.Type renderAppLink apps}
      ''
    }
  }

let refreshCDNSteps = [
  , Step::{
    , name = Some "Install gcloud"
    , uses = Some "GoogleCloudPlatform/github-actions/setup-gcloud@master"
    , `with` = toMap {
        , project_id = "\${{ secrets.GCP_PRODUCTION_PROJECT_ID }}"
        , service_account_key = "\${{ secrets.GCP_PRODUCTION_KEY }}"
        , export_default_credentials = "true"
      }
    }
  , Step::{
      , name = Some "Invalidate CDN cache"
      , run = Some ''
        gcloud compute url-maps invalidate-cdn-cache ksf-frontends-lb --path "/*"
        gcloud compute url-maps invalidate-cdn-cache mitt-konto --path "/*"
      ''
    }
  ]

let uploadSteps
  = \(env : Env)
  -> Prelude.List.map App.Type Step.Type (mkUploadStep env)

let buildSteps = Prelude.List.map App.Type Step.Type mkBuildStep

in
{ Step
, Prelude
, App
, Env
, setupSteps
, buildSteps
, uploadSteps
, checkCIStep
, linkPreviewsStep
, refreshCDNSteps
}