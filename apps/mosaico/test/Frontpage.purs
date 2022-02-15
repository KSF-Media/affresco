module Mosaico.Test.Frontpage where

import Prelude hiding (sub)

import Data.Array (head)
import Data.Maybe (Maybe(..), fromMaybe)
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Mosaico.Test (Test, log, site, sub)
import KSF.Puppeteer as Chrome
import Test.Unit.Assert as Assert

testHtmlEmbed :: Test
testHtmlEmbed page = do
  Chrome.goto (Chrome.URL site) page
  -- Not trying to really control what's included but this seems to be
  -- a prefix they're using and we're not
  Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list *[class^='dre']") page

testHtmlEmbedNavigation :: Test
testHtmlEmbedNavigation page = do
  let logo = (Chrome.Selector ".mosaico-header__logo")
  Chrome.goto (Chrome.URL $ site <> "meny") page
  Chrome.waitFor_ logo page
  Chrome.click logo page
  Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list *[class^='dre']") page

testMostRead :: Boolean -> Test
testMostRead strict page = do
  Chrome.goto (Chrome.URL site) page
  let mostReadList = Chrome.Selector ".mosaico-asidelist__mostread"
      article = Chrome.Selector "article.mosaico-article"
  Chrome.waitFor_ mostReadList page
  Chrome.click (sub " li a" mostReadList) page
  mostRead <- Lettera.responseBody <$> Lettera.getMostRead 0 1 Nothing HBL true
  let firstArticleTitle = fromMaybe "" $ _.title <$> (join <<< map head $ mostRead)
  Assert.assert "Got valid title from Lettera" $ firstArticleTitle /= ""
  Chrome.waitFor_ article page
  -- It's very unlikely, but still possible, that most read list
  -- changed between creating the front page and this.
  if strict
    then Chrome.assertContent (sub " .mosaico-article__headline" article) firstArticleTitle page
    else do
    articleTitle <- Chrome.getContent (sub " .mosaico-article__headline" article) page
    when (articleTitle /= firstArticleTitle) do
      log "Most read title on page didn't match the one from Lettera, retry once"
      testMostRead true page
