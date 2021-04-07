{-

Template to generate the `.github/workflows/previews.yml` CI file

-}

let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let apps = ./apps.dhall

let previewUrl = "https://deploy-previews.ksfmedia.fi/\${{ github.sha }}"

let steps
  = Actions.setupSteps
  # [ Actions.checkCIStep ]
  # Actions.buildSteps apps
  # Actions.uploadSteps Actions.Env.Staging apps
  # [ Actions.linkPreviewsStep apps previewUrl ]

in
  { name = "previews"
  , on = { pull_request = { branches = ["master"] } }
  , jobs = { deploy = { runs-on = "ubuntu-latest", steps = steps } }
  }