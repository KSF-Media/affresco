module Test.Main where

import Prelude

import Bottega (createOrder, payOrder) as Bottega
import Bottega.Models (PaymentMethod(..), PaymentTerminalUrl) as Bottega
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import KSF.Api (UserAuth)
import KSF.Test (getTestCard, getTimeStamp, typeCreditCard)
import MittKonto.Test.Payment as Payment
import MittKonto.Test.Profile as Profile
import MittKonto.Test.Subscription as Subscription
import Persona as Persona
import KSF.Puppeteer as Chrome

main :: Effect Unit
main = launchAff_ do
  dateTimeStr <- liftEffect getTimeStamp
  let customer1 = mkEmail dateTimeStr

  withBrowser \browser -> do
    let password = "myvoiceismypassword"
    log $ ">>> Log in " <> customer1
    auth <- createAccountAndLogin customer1 password
    terminalUrl <- createSubscription auth
    case terminalUrl of
      Just url -> do
        page <- Chrome.newPage browser
        Chrome.goto (Chrome.URL $ url.paymentTerminalUrl) page
        liftEffect (getTestCard 0) >>= typeCreditCard page
        Chrome.waitForNavigation {} page
      _ -> pure unit
    page <- Chrome.newPage browser
    Chrome.goto (Chrome.URL "http://localhost:8000/") page
    inputLogin page customer1 password
    Chrome.waitFor_ (Chrome.Selector ".profile--profile-row:nth-child(1) .profile--edit-text") page
    runTest "change name" Profile.testNameChange page
    runTest "change address" Profile.testAddressChange page
    runTest "change email" Profile.testEmailChange page
    runTest "invoice test" Payment.testInvoice page
    runTest "credit card change test" (Payment.testCreditCardChange auth) page
    -- The first page load may not have had the subscription on the
    -- page yet.  Force a reload.
    Chrome.goto (Chrome.URL "http://localhost:8000/?") page
    runTest "pause subscription" Subscription.testPause page
    runTest "temporary address change" Subscription.testTemporaryAddressChange page
  where
    mkEmail :: String -> String
    mkEmail dateTimeStr = "mittkonto+test." <> dateTimeStr <> "@ksfmedia.fi"

    runTest name test page = do
      log $ ">>> Running test: " <> show name
      test page
      log $ ">>> test successful."

    withBrowser = bracket Chrome.launch Chrome.close

inputLogin :: Chrome.Page -> String -> String -> Aff Unit
inputLogin page email password = do
  let emailField = Chrome.Selector ".login-form .input-field--container input[name='accountEmail']"
      passwordField = Chrome.Selector ".login-form .input-field--container input[name='accountPassword']"
  Chrome.waitFor_ emailField page
  Chrome.type_ emailField email page
  Chrome.type_ passwordField password page
  Chrome.click (Chrome.Selector ".login-form input[type='submit']") page
  Chrome.waitFor_ (Chrome.Selector ".profile--profile-row") page

createAccountAndLogin :: String -> String -> Aff UserAuth
createAccountAndLogin email password = do
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
  loginData <- Persona.login { username: email, password: password, mergeToken: Nullable.null }
  pure { userId: loginData.uuid, authToken: loginData.token }

createSubscription :: UserAuth -> Aff (Maybe Bottega.PaymentTerminalUrl)
createSubscription auth = do
  order <- Bottega.createOrder auth
             { packageId: "HBL_P+D"
             , period: 1
             , payAmountCents: 3990
             , campaignNo: Nothing
             }
  logShow order
  Bottega.payOrder auth order.number Bottega.CreditCard
