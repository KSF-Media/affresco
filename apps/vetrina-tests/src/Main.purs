module Main where

import Prelude

import Data.DateTime (DateTime)
import Data.Formatter.DateTime as Format
import Data.List as List
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now as Now
import Test.Unit.Assert as Assert
import Puppeteer as Chrome


main :: Effect Unit
main = launchAff_ do
  -- setup
  browser <- Chrome.launch
  page <- Chrome.newPage browser
  Chrome.goto (Chrome.URL "https://vetrina-staging.netlify.app") page

  let runTest test = do
        dateTimeStr <- liftEffect $ formatDate <$> Now.nowDateTime
        let email = "fabrizio.ferrai+" <> dateTimeStr <> "@ksfmedia.fi"
        let password = "test123"
        Chrome.goto (Chrome.URL "https://vetrina-staging.netlify.app") page
        test page email password

  -- run all tests here
  runTest subscribeNewCustomer

  -- teardown
  -- log "Waiting for network activity to stop"
  -- Chrome.waitForNavigation { waitUntil: Chrome.networkIdle2 } page
  Chrome.close browser


subscribeNewCustomer :: Test
subscribeNewCustomer page email password = do
  log "Fill in a fresh email, so we create a new account"
  let emailField = Chrome.Selector ".input-field--container > input:nth-child(1)"
  Chrome.waitFor_ emailField page
  Chrome.type_ emailField email page

  log "Accepting terms"
  let termsCheckbox = Chrome.Selector "#accept-terms"
  Chrome.click termsCheckbox page

  log "Submitting the form"
  Chrome.click (Chrome.Selector ".vetrina--button") page
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

  log "Payment should have succeeded, so we can now set the password of the account"
  let passSelector1 = Chrome.Selector "#root > div > div > div > form > div > div:nth-child(1) > input[type=password]"
  let passSelector2 = Chrome.Selector "#root > div > div > div > form > div > div:nth-child(2) > input[type=password]"
  Chrome.waitFor_ passSelector1 page
  Chrome.type_ passSelector1 password page
  Chrome.type_ passSelector2 password page
  Chrome.click (Chrome.Selector "#root > div > div > div > form > input") page

  log "Check that we land on the right page at the end"
  Aff.delay (Milliseconds 1000.0)
  let titleSelector = Chrome.Selector "#root > div > div > h1"
  Chrome.waitFor_ titleSelector page
  theTitle <- Chrome.getContent titleSelector page
  -- void $ Chrome.screenshot {path: "./test" <> dateTimeStr <> ".png" } page
  Assert.equal theTitle "Ditt KSF Media-konto är klart!"


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