module Mosaico.Test.Frontpage where

import Prelude hiding (sub)

import Data.Array (find, head, mapMaybe)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Mosaico.Test (Test, site, sub)
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
      getRelatedTitle i = do
        title <- Chrome.getContent (sub (" li:nth-child(" <> show i <> ") a") mostReadList) page
        pure { i, title }
      matchTitle { i, title } stubs = (\a -> { i, title, a })
                                      <$> find (\s -> title == fromMaybe s.title s.listTitle) stubs
  Chrome.waitFor_ mostReadList page
  -- Mosaico's and Lettera's most read data might not match due to
  -- timing or caching issues.  Satisfy the test if they have at least
  -- one article in common.
  titles <- traverse getRelatedTitle [1,2,3,4,5,6,7,8,9,10]
  mostRead <- fromMaybe [] <<< Lettera.responseBody <$> Lettera.getMostRead 0 10 Nothing HBL true
  let matchingTitles = mapMaybe (\t -> matchTitle t mostRead) titles
  case head matchingTitles of
    Nothing -> failure "No common articles found in Lettera's and Mosaico's most read lists"
    Just {i, a} -> do
      Chrome.click (sub (" li:nth-child(" <> show i <> ") a") mostReadList) page
      Chrome.waitFor_ article page
      Chrome.assertContent (sub " .mosaico-article__headline" article) a.title page
