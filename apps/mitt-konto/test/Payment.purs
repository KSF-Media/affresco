module MittKonto.Test.Payment where

import Prelude

import Bottega as Bottega
import Data.String as String
import Effect.Class (liftEffect)
import Effect.Now as Now
import KSF.Api (UserAuth)
import KSF.Helpers as Helpers
import KSF.Test (getTestCard, typeCreditCard)
import MittKonto.Test (Test)
import Puppeteer as Chrome
import Test.Unit as Test
import Test.Unit.Assert as Assert

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
  Chrome.assertContent (Chrome.Selector ".payment-detail--table tbody tr:first-child th:first-child") "Utskrivningsdatum" page
  Chrome.assertContent (Chrome.Selector ".payment-detail--table tbody tr:first-child td:nth-child(2)") nowText page
  Chrome.assertContent (Chrome.Selector ".payment-detail--table tbody tr:nth-child(3) td:nth-child(2)") "Mannerheimvägen 18, 00100 HELSINGFORS" page
  Chrome.click (Chrome.Selector ".mitt-konto--backwards") page
  checkAccordionPage
  Chrome.click (Chrome.Selector ".mitt-konto--backwards") page
  Chrome.waitFor_ (Chrome.Selector ".profile--profile-row") page
  Chrome.assertNotFound (Chrome.Selector ".payment-accordion--details") page

testCreditCardChange :: UserAuth -> Test
testCreditCardChange auth page = do
  subsno <- Chrome.getContent (Chrome.Selector ".subscription--container dl dd:nth-child(4)") page
  originalCard <- liftEffect $ getTestCard 0
  updatedCard <- liftEffect $ getTestCard 1
  let expectedPan card = (String.take 6 card.number) <> "******" <> (String.drop 12 card.number)
      expectedExpiry card = card.year <> "01"
      netsIframe = Chrome.Selector "iframe.credit-card-register--terminal"
      updateCardLink = Chrome.Selector $ "#subscription-" <> subsno <> " .subscription--credit-card-update-icon"
      testCard expectedCard = do
        cards <- Bottega.getCreditCards auth
        case cards of
          [card] -> do
            Assert.equal (expectedPan expectedCard) card.maskedPan
            Assert.equal (expectedExpiry expectedCard) card.expiryDate
          _ -> Test.failure "Expected exactly one credit card"
  testCard originalCard
  Chrome.waitFor_ updateCardLink page
  Chrome.click updateCardLink page
  frameHandle <- Chrome.waitFor netsIframe page
  iframe <- Chrome.contentFrame frameHandle
  typeCreditCard iframe updatedCard
  Chrome.waitFor_ (Chrome.Selector ".subscription--container") page
  testCard updatedCard
