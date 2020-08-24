{-

Template to generate the `.github/workflows/previews.yml` CI file

-}

let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let apps = ./apps.dhall

let checkCIStep = Actions.Step::{
  , name = Some "Check CI script has been generated from Dhall"
  , run = Some ''
      nix-shell ci/dhall.nix --run 'dhall-to-yaml --omit-empty <<< "./ci/previews-ci.dhall"' > .github/workflows/previews.yml
      git diff --exit-code
      echo "TODO production"
    ''
  }

let linkPreviewsStep = Actions.Step::{
  , name = Some "Post preview links"
  , uses = Some "unsplash/comment-on-pr@master"
  , env = toMap { GITHUB_TOKEN = "\${{ secrets.GITHUB_TOKEN }}" }
  , `with` = toMap {
    , msg =
      let renderAppLink = \(app : Actions.App.Type) -> "- [${app.name}](\${{ env.preview_url }}/${app.deployDir}/index.html)"
      in ''
      Deploy previews:
      ${Prelude.Text.concatMapSep "\n" Actions.App.Type renderAppLink apps}
      ''
    }
  }

let steps
  = Actions.setupSteps
  # [ checkCIStep ]
  # Actions.buildSteps apps
  # Actions.uploadSteps apps
  # [ linkPreviewsStep ]

in
  { name = "previews"
  , on = { pull_request = { branches = ["master"] } }
  , jobs = { deploy = { runs-on = "ubuntu-latest", steps = steps } }
  , env = {
    , bucket_name = "deploy-previews"
    , preview_url = "https://deploy-previews.ksfmedia.fi/\${{ github.sha }}"
    }
  }