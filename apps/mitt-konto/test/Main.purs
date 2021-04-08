module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import KSF.Test (getTimeStamp)
import MittKonto.Test.Profile as Profile
import Persona as Persona
import Puppeteer as Chrome

main :: Effect Unit
main = launchAff_ do
  dateTimeStr <- liftEffect getTimeStamp
  let customer1 = mkEmail dateTimeStr

  withBrowser \browser -> do
    let password = "myvoiceismypassword"
    page <- Chrome.newPage browser
    Chrome.goto (Chrome.URL "http://localhost:8000/") page
    log $ ">>> Log in"
    login page customer1 password
    runTest "change name" Profile.testNameChange page
    runTest "change address" Profile.testAddressChange page
    runTest "change email" Profile.testEmailChange page
  where
    mkEmail :: String -> String
    mkEmail dateTimeStr = "mittkonto+test." <> dateTimeStr <> "@ksfmedia.fi"

    runTest name test page = do
      log $ ">>> Running test: " <> show name
      test page
      log $ ">>> test successful."

    withBrowser = bracket Chrome.launch Chrome.close

login :: Chrome.Page -> String -> String -> Aff Unit
login page email password = do
  logShow =<< Persona.register
    { firstName: "Test"
    , lastName: "Testtest"
    , emailAddress: email
    , password: password
    , confirmPassword: password
    , streetAddress: "Mannerheimintie 18"
    , zipCode: "00100"
    , city: "Helsinki"
    , country: "FI"
    , phone: "1234567890"
    , legalConsents: [{
        dateAccepted: "2020-09-22T12:58:17.691Z",
        consentId: "legal_acceptance_v1",
        screenName: "legalAcceptanceScreen"
      }]
    }
  log "Log in with the created account"
  let emailField = Chrome.Selector ".login-form .input-field--container input[name='accountEmail']"
      passwordField = Chrome.Selector ".login-form .input-field--container input[name='accountPassword']"
  Chrome.waitFor_ emailField page
  Chrome.type_ emailField email page
  Chrome.type_ passwordField password page
  Chrome.click (Chrome.Selector ".login-form input[type='submit']") page
  Chrome.waitFor_ (Chrome.Selector ".profile--profile-row") page
