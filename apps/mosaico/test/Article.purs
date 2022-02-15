module Mosaico.Test.Article where

import Prelude hiding (sub)

import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (runMaybeT, lift)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Mosaico.Test (Test, log, site, sub)
import KSF.Puppeteer as Chrome
import Test.Unit.Assert as Assert

type PageIds =
  { articleId :: Maybe String
  , premiumArticleId :: Maybe String
  }

relatedExample :: String
relatedExample = "d25bde70-a2a1-4ac6-9c38-22f742b799f7"

navigateToNews :: Chrome.Page -> Aff Unit
navigateToNews page = do
  let newsTitle = Chrome.Selector ".mosaico-header__block:nth-child(3) .mosaico-header__section:nth-child(3) .mosaico-header__section-title a"
  Chrome.click (Chrome.Selector ".mosaico-header__icon-button--menu") page
  Chrome.waitFor_ newsTitle page
  Chrome.click newsTitle page

testNewsPage :: Chrome.Page -> Aff PageIds
testNewsPage page = do
  Chrome.goto (Chrome.URL $ site <> "nyheter") page
  let articleList = Chrome.Selector ".mosaico--article-list"
  Chrome.waitFor_ articleList page
  nPremium <- Chrome.countElements articleList
              (Chrome.Selector ".mosaico--list-article[data-premium='1']") page
  nFree <- Chrome.countElements articleList
           (Chrome.Selector ".mosaico--list-article[data-premium='0']") page
  -- TODO It would be very unusual to have all articles on the news
  -- page either premium or free.  But allow for it in the test.
  -- Error if we have neither.
  Assert.assert "At least one article on front page list" $ nPremium + nFree > 0
  let article = Chrome.Selector "article.mosaico-article"
  premiumArticleId <- runMaybeT do
    guard $ nPremium > 0
    lift do
      let item = Chrome.Selector ".mosaico--list-article[data-premium='1']"
      log "Get first premium article from list"
      uuid <- Chrome.getData item "uuid" page
      articleListTest item
      premiumArticleTest article page
      log "Click to news page"
      -- Return to news page
      navigateToNews page
      Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
      pure uuid
  articleId <-runMaybeT do
    guard $ nFree > 0
    lift do
      let item = Chrome.Selector ".mosaico--list-article[data-premium='0']"
      log "Get first free article from list"
      uuid <- Chrome.getData item "uuid" page
      articleListTest item
      -- Test for lack of premium badge
      Chrome.assertNotFound (sub " .mosaico-article__tag-n-share .premium-badge" article) page
      -- Return to front page
      navigateToNews page
      Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
      pure uuid
  pure { premiumArticleId, articleId }
  where
    -- Navigates to the article and leaves page there
    articleListTest :: Chrome.Selector -> Aff Unit
    articleListTest sel = do
      tag <- Chrome.getContent (sub " .mosaico-article__tag" sel) page
      Chrome.click sel page
      let article = Chrome.Selector "article.mosaico-article"
      Chrome.waitFor_ article page
      -- Tag content should match
      Chrome.assertContent (sub " .mosaico-article__tag" article) tag page

-- Test direct loading of an article
testFreeArticle :: String -> Test
testFreeArticle uuid page = do
  Chrome.goto (Chrome.URL $ site <> "artikel/" <> uuid) page
  let article = Chrome.Selector "article.mosaico-article"
  Chrome.waitFor_ article page
  Chrome.waitFor_ (sub " .mosaico-article__main" article) page
  Chrome.assertNotFound (sub " .mosaico-article__tag-n-share .premium-badge" article) page

-- Tests for non-privileged/not logged in user
premiumArticleTest :: Chrome.Selector -> Test
premiumArticleTest sel page = do
  -- Test that we see paywall
  Chrome.waitFor_ (sub " .mosaico-article__main .mosaico-article__body .vetrina--container" sel) page
  -- Test for premium badge
  Chrome.waitFor_ (sub " .mosaico-article__tag-n-share .premium-badge" sel) page
  -- Test that there's at most one element of content
  Chrome.assertNotFound (sub " .mosaico-article__body .article-element:nth-of-type(2)" sel) page

navigateTo :: String -> Chrome.Page -> Aff Unit
navigateTo uuid page = do
  Chrome.goto (Chrome.URL $ site <> "nyheter") page
  Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
  let item = Chrome.Selector $ ".mosaico--list-article[data-uuid='" <> uuid <> "']"
  Chrome.click item page

testPaywallLogin :: Boolean -> String -> String -> String -> (Chrome.Selector -> Int -> Test) -> Test
testPaywallLogin loadDirect uuid user password f page = do
  log $ "Paywall login, direct " <> show loadDirect <> " article " <> uuid
  if loadDirect
    then Chrome.goto (Chrome.URL $ site <> "artikel/" <> uuid) page
    else navigateTo uuid page
  let article = Chrome.Selector "article.mosaico-article"
      login = Chrome.Selector ".mosaico--login-modal"
  premiumArticleTest article page
  -- Edge case: If premium article has only one body element, it
  -- renders with no body elements with paywall
  originalBlocks <- Chrome.countElements article (Chrome.Selector ".mosaico-article__body .article-element") page
  Assert.assert "One or no content blocks shown in paywall" $ originalBlocks < 2
  -- Test login
  Chrome.click (sub " .vetrina--login-callback" article) page
  Chrome.waitFor_ login page
  Chrome.type_ (sub " .input-field--container:nth-of-type(1) input" login) user page
  Chrome.type_ (sub " .input-field--container:nth-of-type(2) input" login) password page
  Chrome.click (sub " input[type=\"submit\"]" login) page
  f article originalBlocks page

testPaywallOpen :: Chrome.Selector -> Int -> Test
testPaywallOpen article originalBlocks page = do
  log "Check article has more content after login"
  Chrome.waitFor_ (sub (" .mosaico-article__body .article-element:nth-of-type("<> show (originalBlocks+1) <>")") article) page
  log "Check that Vetrina is gone"
  Chrome.assertNotFound (sub " .mosaico-article__main .mosaico-article__body .vetrina--container" article) page
  log "Test that opening premium article with the same session via a list shows content"
  navigateToNews page
  navigateToPremium
  -- Same test via loading front page directly
  log "Test opening premium article with new session"
  Chrome.goto (Chrome.URL $ site <> "nyheter") page
  navigateToPremium
  where
    navigateToPremium = do
      log "Navigate to premium"
      let articleList = Chrome.Selector ".mosaico--article-list"
          item = (sub " .mosaico--list-article[data-premium='1']" articleList)
      Chrome.waitFor_ articleList page
      nPremium <- Chrome.countElements articleList item page
      if nPremium == 0 then log "No premium articles on news page, skip navigation test"
        else do
        Chrome.click item page
        -- Test for premium badge
        Chrome.waitFor_ (sub " .mosaico-article__tag-n-share .premium-badge" article) page
        Chrome.assertNotFound (sub " .mosaico--article-fading-body" article) page

testPaywallHolds :: Chrome.Selector -> Int -> Test
testPaywallHolds article originalBlocks page = do
  Chrome.waitFor_ (sub " h1[data-existing-account='1']" article) page
  paywallBlocks <- Chrome.countElements article (Chrome.Selector ".mosaico-article__body .article-element") page
  Assert.assert "Login without entitlements gives displays the same content" $ paywallBlocks == originalBlocks
  Chrome.waitFor_ (sub " .mosaico-article__main .mosaico-article__body .vetrina--container" article) page

testRelated :: Test
testRelated page = do
  Chrome.goto (Chrome.URL $ site <> "artikel/" <> relatedExample) page
  Chrome.waitFor_ (Chrome.Selector "article.mosaico-article .article-element__related") page
  originalTitle <- Chrome.getContent (Chrome.Selector "article.mosaico-article .mosaico-article__headline") page
  Chrome.click (Chrome.Selector "article.mosaico-article .article-element__related a") page
  Chrome.waitFor_ (Chrome.Selector "article.mosaico-article .article-element__html") page
  newTitle <- Chrome.getContent (Chrome.Selector "article.mosaico-article .mosaico-article__headline") page
  Assert.assert "Article title changes after clicking on a related article" $ originalTitle /= newTitle
