module Mosaico.Test.Account where

import Prelude hiding (sub)

import Mosaico.Test (Test, site, sub)
import KSF.Puppeteer as Chrome
import Test.Unit.Assert as Assert

loginLogout :: String -> String -> Test
loginLogout user password page = do
  Chrome.goto (Chrome.URL $ site <> "meny") page
  Chrome.waitFor_ (Chrome.Selector ".mosaico-header__menu-content") page
  -- Do it twice just to test idempotentness
  runTest
  runTest
  where
    runTest = do
      let menu = Chrome.Selector ".mosaico-header__menu-content"
          logoutSelector = sub " .mosaico-header__block:nth-child(1) .mosaico-header__section:nth-child(4) a" menu
          accountSelector = Chrome.Selector ".mosaico-header__account"
          login = Chrome.Selector ".mosaico--login-modal"
      Chrome.waitFor_ logoutSelector page
      Chrome.assertContent logoutSelector "LOGGA IN" page
      Chrome.assertContent accountSelector "LOGGA IN" page
      Chrome.click accountSelector page
      Chrome.waitFor_ login page
      Chrome.type_ (sub " .input-field--container:nth-of-type(1) input" login) user page
      Chrome.type_ (sub " .input-field--container:nth-of-type(2) input" login) password page
      Chrome.click (sub " input[type=\"submit\"]" login) page
      Chrome.waitFor_ (sub "[data-loggedin=\"1\"]" accountSelector) page
      accountText <- Chrome.getContent accountSelector page
      Assert.assert "Login element text has changed" $ accountText /= "LOGGA IN"
      Chrome.waitFor_ logoutSelector page
      Chrome.click logoutSelector page
      Chrome.waitFor_ (sub "[data-login=\"1\"]" accountSelector) page
