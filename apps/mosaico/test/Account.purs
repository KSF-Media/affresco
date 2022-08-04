module Mosaico.Test.Account where

import Prelude hiding (sub)

import Effect.Aff as Aff
import Effect.Aff (Milliseconds(..))
import Mosaico.Test (Test, site, sub)
import KSF.Puppeteer as Chrome
import Test.Unit.Assert as Assert

accountSelector :: Chrome.Selector
accountSelector = Chrome.Selector ".mosaico-header__account"

loginLogout :: String -> String -> Test
loginLogout user password page = do
  Chrome.goto (Chrome.URL $ site <> "meny") page
  Chrome.waitFor_ (Chrome.Selector ".mosaico-header__menu-content") page
  -- Do it twice just to test idempotentness
  runTest
  runTest
  where
    menu = Chrome.Selector ".mosaico-header__menu-content"
    menuItem n = sub (" .mosaico-header__block:nth-child(1) .mosaico-header__section:nth-child(" <> show n <> ") a") menu
    findLogoutSelector n = do
      let sel = menuItem n
      Chrome.waitFor_ sel page
      content <- Chrome.getContent sel page
      case content of
        "LOGGA IN" -> pure sel
        _ -> findLogoutSelector $ n+1
    runTest = do
      logoutSelector <- findLogoutSelector 1
      Chrome.waitFor_ logoutSelector page
      Chrome.assertContent logoutSelector "LOGGA IN" page
      Chrome.assertContent accountSelector "LOGGA IN" page
      login user password accountSelector 2 page
      accountText <- Chrome.getContent accountSelector page
      Assert.assert "Login element text has changed" $ accountText /= "LOGGA IN"
      Chrome.waitFor_ logoutSelector page
      Chrome.click logoutSelector page
      Chrome.waitFor_ (sub "[data-login=\"1\"]" accountSelector) page

login :: String -> String -> Chrome.Selector -> Int -> Test
login user password openSel attempts page = do
  let modal = Chrome.Selector ".mosaico--login-modal"
      passwordField = (sub " input[type=\"password\"]" modal)
  Chrome.click openSel page
  Chrome.waitFor_ modal page
  Chrome.type_ (sub " input[type=\"email\"]" modal) user page
  Chrome.type_ passwordField (password <> "\n") page
  Chrome.waitFor_ (sub "[data-loggedin=\"1\"], .login-form .login--error-msg" accountSelector) page
  errorCount <- Chrome.countElements
                (Chrome.Selector "#app")
                (Chrome.Selector ".login-form .login--error-msg") page
  -- Looks like repeated logins done by the tests may cause a failure.
  -- We don't have real control over this behavior and it's unlikely
  -- to match regular user actions so just work around it.  Trying
  -- again seems to work.
  when (errorCount > 0 && attempts > 0) do
    -- Give it some extra rest
    Aff.delay (Milliseconds 20000.0)
    Chrome.click (Chrome.Selector ".mosaico--login-modal_close") page
    login user password openSel (attempts - 1) page
