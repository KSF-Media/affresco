module Puppeteer
  ( module Puppeteer
  , module Toppokki
  ) where

import Prelude

import Foreign (unsafeFromForeign)
import Toppokki (class HasFrame, Browser, ElementHandle, Page, URL(..), Selector(..), goto, newPage, waitForNavigation, networkIdle2, close, click, contentFrame, select)
import Toppokki as Chrome
import Test.Unit.Assert as Assert
import Effect.Aff as Aff
import Effect.Aff (Aff, Milliseconds(..))

-- | We need to pass this flag, otherwise iframes don't work properly.
-- | See: https://github.com/puppeteer/puppeteer/issues/5123
launch :: Aff Browser
launch = Chrome.launch { headless: true, args: ["--disable-features=site-per-process"] }

waitFor_ :: forall page. HasFrame page => Selector -> page -> Aff Unit
waitFor_ selector frame = void $ waitFor selector frame

waitFor :: forall page. HasFrame page => Selector -> page -> Aff ElementHandle
waitFor selector frame = Chrome.waitForSelector selector { visible: true } frame

type_ :: forall page. HasFrame page => Selector -> String -> page -> Aff Unit
type_ selector text page = Chrome.keyboardType selector text { delay: 10.0 } page

getContent :: Selector -> Page -> Aff String
getContent selector page = do
  theTitleF <- Chrome.unsafePageEval selector "e => e.textContent" page
  pure (unsafeFromForeign theTitleF)

assertContent :: Selector -> String -> Page -> Aff Unit
assertContent selector expected page = do
  -- here we wait because most likely we're trying to do this on a new page load
  Aff.delay (Milliseconds 300.0)
  waitFor_ selector page
  content <- getContent selector page
  Assert.equal content expected