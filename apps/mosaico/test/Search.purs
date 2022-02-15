module Mosaico.Test.Search where

import Prelude hiding (sub)

import Effect.Aff as Aff
import Mosaico.Test (Test, listArticle, log, site, sub)
import KSF.Puppeteer as Chrome
import Test.Unit.Assert as Assert

exampleSearch :: String
exampleSearch = "hbl"

-- Just something that never should match any article
exampleNegativeSearch :: String
exampleNegativeSearch = "vsdlkjfdskfajadskfjaoiefjlksadfjlkdsafjdaslakjf"

buttonField = Chrome.Selector ".mosaico-search button"
searchField = Chrome.Selector ".mosaico-search input"
messageField = Chrome.Selector ".mosaico-search__message"

testSearchNavigation :: Test
testSearchNavigation page = do
  let searchButton = Chrome.Selector ".mosaico-header__icon-button--search"
  Chrome.goto (Chrome.URL site) page
  Chrome.waitFor_ searchButton page
  Chrome.click searchButton page
  testExampleSearch page

testSearchServerRender :: Test
testSearchServerRender page = do
  Chrome.goto (Chrome.URL $ site <> "sök") page
  testExampleSearch page

testExampleSearch :: Test
testExampleSearch page = do
  log "Search is disabled in initial state"
  Chrome.waitFor_ (sub "[disabled]" buttonField) page
  Chrome.type_ (Chrome.Selector ".mosaico-search input") exampleSearch page
  Chrome.assertNotFound (sub "[disabled]" buttonField) page
  Chrome.click buttonField page
  log "Wait for search results"
  Chrome.waitFor_ listArticle page
  Chrome.assertNotFound messageField page

testFailingSearch :: Test
testFailingSearch page = do
  Chrome.goto (Chrome.URL $ site <> "sök") page
  Chrome.waitFor_ searchField page
  Chrome.type_ (Chrome.Selector ".mosaico-search input") exampleNegativeSearch page
  Chrome.click buttonField page
  Chrome.waitFor_ messageField page
  Chrome.assertContent messageField "Inga resultat" page
  Chrome.assertNotFound listArticle page
