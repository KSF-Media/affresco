module Test.Main where

import Prelude

import Data.DateTime (DateTime)
import Data.Formatter.DateTime as Format
import Data.List as List
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Now as Now
import Persona as Persona
import Puppeteer as Chrome

-- | Run all tests here
main :: Effect Unit
main = launchAff_ do
  -- Setup
  dateTimeStr <- liftEffect $ formatDate <$> Now.nowDateTime
  let customer1 = mkEmail dateTimeStr
  let customer2 = mkEmail (dateTimeStr <> "-2")
  let customer3 = mkEmail (dateTimeStr <> "-3")

  -- First of all we just try to subscribe as a fresh customer, so everything from scratch
  withBrowser (runTest "subscribe new customer" subscribeNewCustomer customer1)

  -- Then we open another browser (otherwise we'd be logged in with the first customer)
  withBrowser \browser -> do
    -- ..and we buy the package with an existing customer (that we create on the
    -- fly just with the Persona API)
    runTest "subscribe with existing (non-entitled) customer" buyWithExistingCustomer customer2 browser
    -- ..and after that we just try to visit again, and verify that Vetrina
    -- figures out that we already have a subscription. Good.
    runTest "visit with entitled user" navigateLoggedIn customer2 browser

  -- Then we try to login from scratch with the customer of the previous step,
  -- and verify that Vetrina figures out that we already have (1) and account
  -- and (2) a subscription
  withBrowser (runTest "visit with entitled user (from scratch)" loginWithEntitledCustomer customer2)

  where
    mkEmail :: String -> String
    mkEmail dateTimeStr = "zztelefon+test." <> dateTimeStr <> "@ksfmedia.fi"

    runTest name test email browser = do
      log $ ">>> Running test: " <> show name
      page <- Chrome.newPage browser
      let password = "test123"
      Chrome.goto (Chrome.URL "http://localhost:8000") page
      test page email password
      log $ ">>> Test successful."

    withBrowser action = do
      browser <- Chrome.launch
      action browser
      Chrome.close browser


subscribeNewCustomer :: Test
subscribeNewCustomer page email password = do
  log $ "Fill in a fresh email, so we create a new account: " <> email
  fillEmail email page
  payWithNets page

  log "Payment should have succeeded, so we can now set the password of the account"
  let passSelector1 = Chrome.Selector "#setPassword > div:nth-child(1) > input[type=password]"
  let passSelector2 = Chrome.Selector "#setPassword > div:nth-child(2) > input[type=password]"
  Chrome.waitFor_ passSelector1 page
  Chrome.type_ passSelector1 password page
  Chrome.type_ passSelector2 password page
  Chrome.click (Chrome.Selector "input[type=\"submit\"]") page

  let titleSelector = Chrome.Selector ".vetrina--headline"
  log "Check that we land on the right page at the end"
  Chrome.assertContent titleSelector "Ditt KSF Media-konto är klart!" page

subscribe365NewCustomer :: Test
subscribe365NewCustomer page email password = do
  log "Selecting HBL 365"
  let selector365 = Chrome.Selector "#HBL\\ 365"
  Chrome.waitFor_ selector365 page
  Chrome.click selector365 page
  subscribeNewCustomer page email password

buyWithExistingCustomer :: Test
buyWithExistingCustomer page email password = do
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
  loginExistingUser password page
  payWithNets page

  let titleSelector = Chrome.Selector "#app > div > h1"
  log "Check that we land on the right page at the end"
  Chrome.assertContent titleSelector "Tack för din beställning!" page

navigateLoggedIn :: Test
navigateLoggedIn page email password = do
  log "This same customer should be already be logged in, so we should we welcomed back here, let's move on"
  let submit = Chrome.Selector "#app > div > form > input"
  Chrome.waitFor_ submit page
  Chrome.click submit page
  assertUserHasSubscription page


loginWithEntitledCustomer :: Test
loginWithEntitledCustomer page email password = do
  log "This same customer should already have an entitlement, so we just login now"
  fillEmail email page
  loginExistingUser password page
  assertUserHasSubscription page


type Test = Chrome.Page -> String -> String -> Aff Unit

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
  let emailField = Chrome.Selector ".input-field--container > input[type=\"email\"]"
  Chrome.waitFor_ emailField page
  Chrome.type_ emailField email page
  log "Accepting terms"
  let termsCheckbox = Chrome.Selector "#accept-terms"
  Chrome.click termsCheckbox page
  log "Submitting the form"
  Chrome.click (Chrome.Selector ".vetrina--button") page

loginExistingUser :: String -> Chrome.Page -> Aff Unit
loginExistingUser password page = do
  log "We then get presented with the screen that tells us that we already have an account"
  Chrome.assertContent (Chrome.Selector "#app > div > h1") "Du har redan ett KSF Media-konto" page

  log "Well, then we fill the password in and we login"
  let passwordSelector = Chrome.Selector "#app > div > form > div:nth-child(2) > div > input[type=password]"
  Chrome.waitFor_ passwordSelector page
  Chrome.type_ passwordSelector password page
  Chrome.click (Chrome.Selector "#app > div > form > input") page

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

assertUserHasSubscription :: Chrome.Page -> Aff Unit
assertUserHasSubscription page = do
  log "Vetrina should have checked that we are entitled"
  let titleSelector = Chrome.Selector ".vetrina--headline"
  Chrome.assertContent titleSelector "Du har redan en prenumeration" page

  log "We then click on 'whatever go ahead' and we should be done"
  Chrome.click (Chrome.Selector ".vetrina--completed-close") page
