module Mosaico.Test.Embeds where

import Prelude hiding (sub)

import Effect.Aff as Aff
import Mosaico.Test (Test, site, sub)
import Puppeteer as Chrome
import Test.Unit.Assert as Assert

exampleArticle :: String
exampleArticle = "97272947-6d4b-42d4-a907-e1a83f265963"

testEmbedNavigation :: Test
testEmbedNavigation page = do
  Chrome.goto (Chrome.URL $ site <> "debug/" <> exampleArticle) page
  Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
  Chrome.click (Chrome.Selector ".mosaico--article-list .mosaico--list-article") page
  Chrome.waitFor_ (Chrome.Selector "div.article-element__html iframe") page

testEmbedServerRender :: Test
testEmbedServerRender page = do
  Chrome.goto (Chrome.URL $ site <> "artikel/" <> exampleArticle) page
  Chrome.waitFor_ (Chrome.Selector "div.article-element__html iframe") page
