module Test.Main where

import Prelude

import Data.DateTime (DateTime)
import Data.Formatter.DateTime as Format
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Now as Now
import Persona as Persona
import Puppeteer as Chrome


main :: Effect Unit
main = launchAff_ do
  let mkEmail :: String -> Maybe String -> String
      mkEmail dateTimeStr extra = "fabrizio.ferrai+" <> dateTimeStr <> maybe "" ("-" <> _) extra <> "@ksfmedia.fi"

  let runTest test browser = do
        page <- Chrome.newPage browser
        dateTimeStr <- liftEffect $ formatDate <$> Now.nowDateTime
        let password = "test123"
        Chrome.goto (Chrome.URL "https://vetrina-staging.netlify.app") page
        test page (mkEmail dateTimeStr) password

  let withBrowser action = do
        browser <- Chrome.launch
        action browser
        Chrome.close browser

  -- run all tests here
  -- runTest subscribeNewCustomer
  withBrowser (runTest buyWithExistingCustomer)
  withBrowser (runTest loginWithEntitledCustomer)



subscribeNewCustomer :: Test
subscribeNewCustomer page mkEmail password = do
  let email = mkEmail Nothing
  log $ "Fill in a fresh email, so we create a new account: " <> email
  fillEmail email page
  payWithNets page

  log "Payment should have succeeded, so we can now set the password of the account"
  let passSelector1 = Chrome.Selector "#root > div > div > div > form > div > div:nth-child(1) > input[type=password]"
  let passSelector2 = Chrome.Selector "#root > div > div > div > form > div > div:nth-child(2) > input[type=password]"
  Chrome.waitFor_ passSelector1 page
  Chrome.type_ passSelector1 password page
  Chrome.type_ passSelector2 password page
  Chrome.click (Chrome.Selector "#root > div > div > div > form > input") page

  let titleSelector = Chrome.Selector "#root > div > div > h1"
  log "Check that we land on the right page at the end"
  Chrome.assertContent titleSelector "Ditt KSF Media-konto är klart!" page

buyWithExistingCustomer :: Test
buyWithExistingCustomer page mkEmail password = do
  let email = mkEmail (Just "new")
  log $ "We have to create a new account first, we can just call to Persona, email: " <> email
  logShow =<< Persona.register
    { firstName: "Testi"
    , lastName: "Testinen"
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
  log "Then we can fill in the email we just registered"
  fillEmail email page

  log "We then get presented with the screen that tells us that we already have an account"
  Chrome.assertContent (Chrome.Selector "#root > div > div > h1") "Du har redan ett KSF Media-konto" page

  log "Well, then we fill the password in and we login"
  let passwordSelector = Chrome.Selector "#root > div > div > form > div:nth-child(2) > div > input[type=password]"
  Chrome.waitFor_ passwordSelector page
  Chrome.type_ passwordSelector password page
  Chrome.click (Chrome.Selector "#root > div > div > form > input") page

  payWithNets page

  let titleSelector = Chrome.Selector "#root > div > div > h1"
  log "Check that we land on the right page at the end"
  Chrome.assertContent titleSelector "Tack för din beställning!" page

loginWithEntitledCustomer :: Test
loginWithEntitledCustomer page mkEmail password = do
  let email = mkEmail (Just "new")
  log "This same customer should already have an entitlement, so we just login now"
  fillEmail email page

  -- TODO: there's a bug here! A customer can buy twice
  Chrome.waitForNavigation { waitUntil: Chrome.networkIdle2 } page





type Test = Chrome.Page -> (Maybe String -> String) -> String -> Aff Unit

formatDate :: DateTime -> String
formatDate = Format.format $ List.fromFoldable
  [ Format.YearFull
  , Format.MonthTwoDigits
  , Format.DayOfMonthTwoDigits
  , Format.Hours24
  , Format.MinutesTwoDigits
  , Format.SecondsTwoDigits
  ]

fillEmail :: forall page. Chrome.HasFrame page => String -> page -> Aff Unit
fillEmail email page = do
  let emailField = Chrome.Selector ".input-field--container > input:nth-child(1)"
  Chrome.waitFor_ emailField page
  Chrome.type_ emailField email page
  log "Accepting terms"
  let termsCheckbox = Chrome.Selector "#accept-terms"
  Chrome.click termsCheckbox page
  log "Submitting the form"
  Chrome.click (Chrome.Selector ".vetrina--button") page

payWithNets :: forall page. Chrome.HasFrame page => page -> Aff Unit
payWithNets page = do
  log "Getting the nets iframe, so we can pay the order"
  let netsIframe = Chrome.Selector "iframe.vetrina--payment-terminal"
  frameHandle <- Chrome.waitFor netsIframe page
  iframe <- Chrome.contentFrame frameHandle
  let netsSubmit = Chrome.Selector "#nextButton"
  log "Waiting for submit button"
  Chrome.waitFor_ netsSubmit iframe
  Chrome.click netsSubmit iframe

  log "Waiting for credit card fields and typing credit card details"
  let creditCardNumber = "4925000000000004"
  let creditCardField = Chrome.Selector "#cardNumber"
  Chrome.waitFor_ creditCardField iframe
  Chrome.type_ creditCardField creditCardNumber iframe
  Chrome.select (Chrome.Selector "#year") "50" iframe
  Chrome.type_ (Chrome.Selector "#securityCode") "666" iframe
  Chrome.click (Chrome.Selector "#okButton") iframe