module MittKonto.Test.Payment where

import Prelude

import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now as Now
import KSF.Helpers as Helpers
import MittKonto.Test (Test)
import Puppeteer as Chrome

testInvoice :: Test
testInvoice page = do
  now <- liftEffect Now.nowDate
  let nowText = Helpers.formatDateDots now
      checkAccordionPage = do
        Chrome.waitFor_ (Chrome.Selector ".payment-accordion--header") page
        Chrome.assertNotFound (Chrome.Selector ".payment-accordion--details") page
        Chrome.assertNotFound (Chrome.Selector ".profile--profile-row") page
        Chrome.assertContent (Chrome.Selector ".payment-accordion--header span:first-child") "HBL HUFVUDSTADSBLADET" page
  Chrome.waitFor_ (Chrome.Selector ".account-edit--payment-history") page
  Chrome.click (Chrome.Selector ".account-edit--payment-history") page
  checkAccordionPage
  Chrome.click (Chrome.Selector ".payment-accordion--header") page
  Chrome.waitFor_ (Chrome.Selector ".payment-accordion--header") page
  Chrome.assertContent (Chrome.Selector ".payment-accordion--item td") nowText page
  Chrome.click (Chrome.Selector ".payment-accordion--item td") page
  Chrome.waitFor_ (Chrome.Selector ".payment-detail--table") page
  Chrome.assertContent (Chrome.Selector ".payment-detail--table tbody tr:first-child th:first-child") "Betaldatum" page
  Chrome.assertContent (Chrome.Selector ".payment-detail--table tbody tr:first-child td:nth-child(2)") nowText page
  Chrome.assertContent (Chrome.Selector ".payment-detail--table tbody tr:nth-child(3) td:nth-child(2)") "Mannerheimvägen 18, 00100 HELSINGFORS" page
  Chrome.click (Chrome.Selector ".mitt-konto--backwards") page
  checkAccordionPage
  Chrome.click (Chrome.Selector ".mitt-konto--backwards") page
  Chrome.waitFor_ (Chrome.Selector ".profile--profile-row") page
  Chrome.assertNotFound (Chrome.Selector ".payment-accordion--details") page
