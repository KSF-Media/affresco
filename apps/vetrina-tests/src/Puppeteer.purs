module Puppeteer
  ( module Puppeteer
  , module Toppokki
  ) where

import Prelude

import Effect.Aff (Aff)
import Foreign (unsafeFromForeign)
import Toppokki
  ( class HasFrame, Browser, ElementHandle, Page, URL(..), Selector(..)
  , goto, newPage, waitForNavigation, networkIdle2, close, click, contentFrame
  , select
  )
import Toppokki as Chrome

-- | We need to pass this flag, otherwise iframes don't work properly.
-- | See: https://github.com/puppeteer/puppeteer/issues/5123
launch :: Aff Browser
launch = Chrome.launch { headless: false, args: ["--disable-features=site-per-process"] }

waitFor_ :: forall page. HasFrame page => Selector -> page -> Aff Unit
waitFor_ selector frame = void $ waitFor selector frame

waitFor :: forall page. HasFrame page => Selector -> page -> Aff ElementHandle
waitFor selector frame = Chrome.waitForSelector selector { visible: true } frame

type_ :: forall page. HasFrame page => Selector -> String -> page -> Aff Unit
type_ selector text page = Chrome.keyboardType selector text { delay: 10.0 } page

getContent :: Selector -> Page -> Aff String
getContent selector page = do
  theTitleF <- Chrome.unsafePageEval selector "e => e.textContent" page
  unsafeFromForeign theTitleF