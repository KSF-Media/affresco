module Mosaico.Test.Static where

import Prelude hiding (sub)

import Mosaico.Test (Test, log, site)
import KSF.Puppeteer as Chrome
import Test.Unit.Assert as Assert

testNavigateToStatic :: Test
testNavigateToStatic page = do
  Chrome.goto (Chrome.URL site) page
  Chrome.waitFor_ (Chrome.Selector ".mosaico--footer .mosaico-footer__links") page
  -- Bruksvillkor
  log "Navigate to bruksvillkor"
  Chrome.click (Chrome.Selector ".mosaico--footer .mosaico-footer__links a:nth-child(2)") page
  Chrome.waitFor_ (Chrome.Selector ".terms__text") page
  log "Navigate to front page"
  Chrome.click (Chrome.Selector ".mosaico-header__logo") page
  Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
  Chrome.back page
  Chrome.waitFor_ (Chrome.Selector ".terms__text") page

testStaticEmbeds :: Test
testStaticEmbeds page = do
  Chrome.goto (Chrome.URL $ site <> "sida/fragor-och-svar") page
  testQuestion
  log "Navigate to front page and back"
  Chrome.click (Chrome.Selector ".mosaico-header__logo") page
  Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
  Chrome.back page
  testQuestion
  where
    testQuestion = do
      log "Questions start as unopened"
      Chrome.waitFor_ (Chrome.Selector ".faq__question") page
      firstQText <- Chrome.getContent (Chrome.Selector ".faq__question") page
      Assert.assert "First Q text is non-empty" $ firstQText /= ""
      Chrome.assertNotFound (Chrome.Selector ".faq__question--active") page
      log "Open first question"
      Chrome.click (Chrome.Selector ".faq__question") page
      Chrome.waitFor_ (Chrome.Selector ".faq__question--active") page
      activeQText <- Chrome.getContent (Chrome.Selector ".faq__question--active") page
      Assert.assert "First Q text matches with active Q text" $ firstQText == activeQText
