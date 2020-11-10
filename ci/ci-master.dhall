{-

Template to generate the `.github/workflows/production.yml` CI file

-}

let Prelude = ./Prelude.dhall

let Actions = ./workflows.dhall

let apps = ./apps.dhall

let steps
  = Actions.setupSteps
  # [ Actions.checkCIStep ]
  # Actions.buildSteps apps
  # Actions.uploadSteps Actions.Env.Production apps
  # Actions.refreshCDNSteps

in
  { name = "production"
  , on = { push = { branches = [ "master" ] } }
  , jobs = { deploy = { runs-on = "ubuntu-latest", steps = steps } }
  , env = {
    , PRODUCTION_FACEBOOK_APP_ID = "1742585382627694"
    , PRODUCTION_GOOGLE_CLIENT_ID = "584250859572-po558qgkq0b4u3j6a1ge40scjhops3oo"
    , PRODUCTION_JANRAIN_FLOW_VERSION = "20161013180819779934"
    , PRODUCTION_JANRAIN_LOGIN_CLIENT_ID = "jzbqvf3p68e8hje6wrzhezk98m8qndmz"
    , PRODUCTION_JANRAIN_SSO_SERVER = "https://ksf-media.eu.janrainsso.com"
    , PRODUCTION_JANRAIN_XD_RECEIVER_PATH = "/xd_receiver.html"
    , PRODUCTION_BOTTEGA_URL = "https://bottega.api.ksfmedia.fi/v1/"
    , PRODUCTION_PERSONA_URL = "https://persona.api.ksfmedia.fi/v1/"
    , PRODUCTION_LETTERA_URL = "https://lettera.api.ksfmedia.fi/v1/"
    , PRODUCTION_DUELLEN_URL = "https://duellen.api.ksfmedia.fi/"
    }
  }