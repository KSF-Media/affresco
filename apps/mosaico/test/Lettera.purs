-- Tests which compare rendered data to what's got from Lettera
module Mosaico.Test.Lettera where

import Prelude hiding (sub)

import Data.Array (head, length, zip, (..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix, toUpper)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Lettera.Models (Category(..), CategoryLabel(..), CategoryType(..))
import Lettera.Models as Models
import Mosaico.Test (Test, listArticle, log, site, sub)
import Partial.Unsafe (unsafePartial)
import KSF.Puppeteer as Chrome
import Test.Unit as Unit
import Test.Unit.Assert as Assert

listTitleExample :: UUID
listTitleExample = unsafePartial $ fromJust $ UUID.parseUUID "6555d4ff-cbd0-4a85-ac60-4967166704a4"

missingListTitleExample :: UUID
missingListTitleExample = unsafePartial $ fromJust $ UUID.parseUUID "f4b6ca52-6078-43c8-b31f-9df85087dbff"

testListTitle :: Test
testListTitle page = do
  Chrome.goto (Chrome.URL $ site <> "debug/" <> UUID.toString listTitleExample) page
  maybeStub <- Lettera.getArticleStub listTitleExample
  case maybeStub of
    Left err ->
      Unit.failure $ "Fetching example article stub failed: " <> err
    Right stub -> do
      Chrome.waitFor_ listArticle page
      log "List uses listTitle"
      -- The example should have a listTitle
      Chrome.assertContent (sub " h2" listArticle) (fromMaybe "☃INVALID" stub.listTitle) page
      Chrome.click listArticle page
      log "Article uses title"
      Chrome.waitFor_ (Chrome.Selector "article.mosaico-article") page
      Chrome.assertContent (Chrome.Selector "article.mosaico-article h1.mosaico-article__headline") stub.title page
      log "Article uses title after full load"
      Chrome.waitFor_ (Chrome.Selector "article.mosaico-article .mosaico-article__body *[class^='article-element']") page
      Chrome.assertContent (Chrome.Selector "article.mosaico-article h1.mosaico-article__headline") stub.title page

testDefaultListTitle :: Test
testDefaultListTitle page = do
  Chrome.goto (Chrome.URL $ site <> "debug/" <> UUID.toString missingListTitleExample) page
  maybeStub <- Lettera.getArticleStub missingListTitleExample
  case maybeStub of
    Left err ->
      Unit.failure $ "Fetching example article stub failed: " <> err
    Right stub -> do
      Chrome.waitFor_ listArticle page
      log "List defaults to using title"
      Chrome.assertContent (sub " h2" listArticle) stub.title page

-- TODO do this for other papers as well

-- TODO there is a (very small?) chance that the feed would be changed
-- between the render and Lettera fetch.  If that becomes an issue run
-- this test twice if it fails once
testCategoryLists :: Test
testCategoryLists page = do
  Chrome.goto (Chrome.URL $ site <> "meny") page
  categories <- Lettera.getCategoryStructure HBL
  traverse_ testCategory $ zip (1..length categories) categories
  where
    testCategory (Tuple idx (Category c))
      | c.type == Feed = do
        log $ "test feed category " <> show c.label
        firstArticle <- join <<< map head <<< Lettera.responseBody <$>
                        (Lettera.getFrontpage HBL (Just $ show c.label) Nothing)
        let catElement = Chrome.Selector $ ".mosaico-header__block:nth-child(3) .mosaico-header__section:nth-of-type(" <> show idx <> ") a"
            (CategoryLabel catLabel) = c.label
        Chrome.waitFor_ catElement page
        Chrome.assertContent catElement (toUpper catLabel) page
        Chrome.click catElement page
        Chrome.waitFor_ listArticle page
        uuid <- stripPrefix (Pattern "/artikel/")
                <$> Chrome.getHref (sub " a[href^='/artikel/']" listArticle) page
        Assert.assert "UUID not found" $ isJust uuid
        Assert.assert "Mismatch with page's and feed's UUIDs" $ (_.uuid <$> firstArticle) == uuid
        Chrome.assertContent (sub " h2" listArticle)
          (fromMaybe "☃INVALID" $ (\x -> fromMaybe x.title x.listTitle) <$> firstArticle) page
        Chrome.goto (Chrome.URL $ site <> "meny") page
      | otherwise = do
        log $ "No test for category type " <> Models.toString c.type <> " (" <> show c.label <> ")"
