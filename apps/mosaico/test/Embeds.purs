module Mosaico.Test.Embeds where

import Prelude hiding (sub)

import Effect.Aff as Aff
import Mosaico.Test (Test, log, site, sub)
import Puppeteer as Chrome

exampleArticle :: String
exampleArticle = "97272947-6d4b-42d4-a907-e1a83f265963"

testEmbedNavigation :: Test
testEmbedNavigation page = do
  log "Get debug list with example article"
  Chrome.goto (Chrome.URL $ site <> "debug/" <> exampleArticle) page
  Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
  log "Navigate to test article"
  Chrome.click (Chrome.Selector ".mosaico--article-list .mosaico--list-article") page
  log "Wait for embed content"
  Chrome.waitFor_ (Chrome.Selector "div.article-element__html iframe") page

testEmbedServerRender :: Test
testEmbedServerRender page = do
  log "Go directly to embed article"
  Chrome.goto (Chrome.URL $ site <> "artikel/" <> exampleArticle) page
  log "Wait for embed content"
  Chrome.waitFor_ (Chrome.Selector "div.article-element__html iframe") page
