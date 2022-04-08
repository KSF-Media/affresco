module Mosaico.Test.Frontpage where

import Prelude hiding (sub)

import Data.Array (head)
import Data.Maybe (Maybe(..), fromMaybe)
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Mosaico.Test (Test, matchTagList, site, sub, tagListWithSelector)
import KSF.Puppeteer as Chrome
import Test.Unit (failure)

testHtmlEmbed :: Test
testHtmlEmbed page = do
  Chrome.goto (Chrome.URL site) page
  -- Not trying to really control what's included but this seems to be
  -- a prefix they're using and we're not
  Chrome.waitFor_ (Chrome.Selector ".mosaico-main *[class^='dre']") page

testHtmlEmbedNavigation :: Test
testHtmlEmbedNavigation page = do
  let logo = (Chrome.Selector ".mosaico-header__logo")
  Chrome.goto (Chrome.URL $ site <> "meny") page
  Chrome.waitFor_ logo page
  Chrome.click logo page
  Chrome.waitFor_ (Chrome.Selector ".mosaico-main *[class^='dre']") page

testMostRead :: Test
testMostRead page = do
  Chrome.goto (Chrome.URL site) page
  let mostReadList = Chrome.Selector ".mosaico-asidelist__mostread"
      article = Chrome.Selector "article.mosaico-article"
      getRelatedTitle i =
        Chrome.getContent (sub (" li:nth-child(" <> show i <> ") a") mostReadList) page
      matchTitle title stub =
        title == fromMaybe stub.title stub.listTitle
  Chrome.waitFor_ mostReadList page
  titles <- tagListWithSelector 10 getRelatedTitle
  mostRead <- fromMaybe [] <<< Lettera.responseBody <$> Lettera.getMostRead 0 10 Nothing HBL true
  let matchingTitles = matchTagList titles mostRead matchTitle
  case head matchingTitles of
    Nothing -> failure "No common articles found in Lettera's and Mosaico's most read lists"
    Just {i, match} -> do
      Chrome.click (sub (" li:nth-child(" <> show i <> ") a") mostReadList) page
      Chrome.waitFor_ article page
      Chrome.assertContent (sub " .mosaico-article__headline" article) match.title page
