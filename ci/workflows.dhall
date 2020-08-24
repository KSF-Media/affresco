let Prelude = ./Prelude.dhall

let Map = Prelude.Map.Type

let App =
  { Type = {
    , buildDir : Text
    , deployDir : Text
    , name : Text
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
      yarn install
      mkdir -p build
    ''
  }
]

-- nix-shell ci/dhall.nix --run 'dhall-to-yaml "./ci/previews-ci.dhall"'

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
  = \(app : App.Type)
  -> Step::{
    , name = Some "Upload ${app.name}"
    , uses = Some "GoogleCloudPlatform/github-actions/upload-cloud-storage@master"
    , `with` = toMap {
      , path = "build/${app.deployDir}"
      , destination = "\${{ env.bucket_name }}/\${{ github.sha }}"
      , credentials = "\${{ secrets.GCP_PREVIEW_KEY }}"
    }
  }

let buildSteps = Prelude.List.map App.Type Step.Type mkBuildStep

let uploadSteps = Prelude.List.map App.Type Step.Type mkUploadStep

in
{ Step = Step
, Prelude = Prelude
, App = App
, setupSteps = setupSteps
, buildSteps = buildSteps
, uploadSteps = uploadSteps
}