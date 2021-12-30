module Mosaico.Test.Tags where

import Prelude hiding (sub)

import Mosaico.Test (Test, listArticle, log, site, sub)
import Puppeteer as Chrome

exampleTaglessArticle :: String
exampleTaglessArticle = "c2a50197-76ae-40df-be7b-b903bac042f4"

-- Hopefully always populated
exampleTag :: String
exampleTag = "helsingfors"

testTaglessArticleNavigation :: Test
testTaglessArticleNavigation page = do
  Chrome.goto (Chrome.URL $ site <> "debug/" <> exampleTaglessArticle) page
  log "Article should be without tag in list"
  Chrome.assertNotFound (sub " .mosaico-article__tag" listArticle) page
  Chrome.click listArticle page
  testTaglessArticle page

testTaglessArticleServerRender :: Test
testTaglessArticleServerRender page = do
  Chrome.goto (Chrome.URL $ site <> "artikel/" <> exampleTaglessArticle) page
  testTaglessArticle page

testTaglessArticle :: Test
testTaglessArticle page = do
  Chrome.waitFor_ (Chrome.Selector "article.mosaico-article") page
  log "Article should be without tag"
  Chrome.assertNotFound (Chrome.Selector "article.mosaico-article .mosaico-article__tag") page

testTagList :: Test
testTagList page = do
  Chrome.goto (Chrome.URL $ site <> "tagg/" <> exampleTag) page
  log "Article in list has tag"
  Chrome.waitFor_ (sub " .mosaico-article__tag" listArticle) page
  Chrome.click listArticle page
  Chrome.waitFor_ (Chrome.Selector "article.mosaico-article") page
  -- TODO: This may fail due to colon bug in list fetch by tag
  log "Tag list can be opened from a tag in an article and it is populated"
  Chrome.click (Chrome.Selector "article.mosaico-article .mosaico-article__tag") page
  Chrome.waitFor_ listArticle page
