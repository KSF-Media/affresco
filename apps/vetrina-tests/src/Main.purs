module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, error)
import Toppokki as Chrome

main :: Effect Unit
main = launchAff_ do
  -- We need to pass this flag, otherwise iframes don't work properly.
  -- See: https://github.com/puppeteer/puppeteer/issues/5123
  browser <- Chrome.launch { headless: false, args: ["--disable-features=site-per-process"] }
  page <- Chrome.newPage browser
  Chrome.goto (Chrome.URL "https://vetrina-staging.netlify.app") page
  let keebOptions = { delay: 10.0 }
  let waitOptions = { waitUntil: Chrome.networkIdle2 }
  let wait = do
        log "Waiting for network activity to stop"
        Chrome.waitForNavigation waitOptions page

  -- fill in non-registered email
  let emailField = Chrome.Selector ".input-field--container > input:nth-child(1)"
  let dateTimeStr = "202007312035" -- TODO
  let unregisteredEmail = "fabrizio.ferrai+" <> dateTimeStr <> "@ksfmedia.fi"
  void $ Chrome.waitForSelector emailField {} page
  log "Filling in email"
  Chrome.keyboardType emailField unregisteredEmail keebOptions page
  -- accept villkor
  log "Accepting terms"
  let termsCheckbox = Chrome.Selector "#accept-terms"
  Chrome.click termsCheckbox page
  -- submit
  void $ Chrome.screenshot {path: "./test" <> dateTimeStr <> ".png" } page
  log "Submitting the form"
  Chrome.click (Chrome.Selector ".vetrina--button") page
  -- then we should get to the nets form to pay
  let netsIframe = Chrome.Selector "iframe.vetrina--payment-terminal"
  log "Waiting for nets iframe"
  frameHandle <- Chrome.waitForSelector netsIframe {visible: true} page
  iframe <- Chrome.contentFrame frameHandle
  let netsSubmit = Chrome.Selector "#nextButton"
  log "Waiting for submit button"
  void $ Chrome.waitForSelector netsSubmit {} iframe
  log "Taking screenshot"
  void $ Chrome.screenshot {path: "./test2.png"} page
  log "Clicking submit button"
  Chrome.click netsSubmit iframe
  let creditCardNumber = "4925000000000004"
  let creditCardField = Chrome.Selector "#cardNumber"
  log "Waiting for credit card fields"
  void $ Chrome.waitForSelector creditCardField {visible:true} iframe
  log "Typing the credit card details"
  Chrome.keyboardType creditCardField creditCardNumber keebOptions iframe

-- #year
-- option value 50

  wait

  Chrome.close browser