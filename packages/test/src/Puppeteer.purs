module KSF.Puppeteer
  ( module KSF.Puppeteer
  , module Toppokki
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array (replicate)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.String as String
import Data.Foldable (sequence_)
import Foreign (unsafeFromForeign)
import Toppokki (class HasFrame, Browser, ElementHandle, Page, URL(..), Selector(..), goto, newPage, waitForNavigation, networkIdle2, close, click, contentFrame, select)
import Toppokki as Chrome
import Test.Unit as Test
import Test.Unit.Assert as Assert
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Exception (throwException)

foreign import headless :: Boolean

-- | We need to pass this flag, otherwise iframes don't work properly.
-- | See: https://github.com/puppeteer/puppeteer/issues/5123
launch :: Aff Browser
launch = Chrome.launch 
  { headless
  , args: [ "--disable-features=site-per-process"
          , "--disable-gpu"
          , "--disable-dev-shm-usage"
          , "--disable-setuid-sandbox"
          , "--no-sandbox"
          ] 
  }

launchDesktop :: Aff Browser
launchDesktop = Chrome.launch
                { headless
                , args: ["--disable-features=site-per-process"
                        , "--disable-gpu"
                        , "--disable-dev-shm-usage"
                        , "--disable-setuid-sandbox"
                        , "--no-sandbox"
                        ] 
                , defaultViewport: { deviceScaleFactor: 1.0
                                   , hasTouch: false
                                   , width:1920.0
                                   , height: 1080.0
                                   , isLandscape: true
                                   , isMobile: false
                                   }
                }

waitFor_ :: forall page. HasFrame page => Selector -> page -> Aff Unit
waitFor_ selector frame = void $ waitFor selector 30000 frame

waitForLong_ :: forall page. HasFrame page => Selector -> page -> Aff Unit
waitForLong_ selector frame = void $ waitFor selector 100000 frame

waitFor :: forall page. HasFrame page => Selector -> Int -> page -> Aff ElementHandle
waitFor selector timeout frame = Chrome.waitForSelector selector { visible: true, timeout } frame

foreign import _xpathEval :: forall page. Fn2 page String (Effect (Promise (Array ElementHandle)))
foreign import _clickElement :: ElementHandle -> Effect (Promise Unit)
foreign import _goto_options :: forall page options. Fn3 URL page options (Effect (Promise Unit))

xpathEval :: forall page. HasFrame page => String -> page -> Aff (Array ElementHandle)
xpathEval xpath page = toAffE $ runFn2 _xpathEval page xpath

clickElement :: ElementHandle -> Aff Unit
clickElement elem = toAffE $ _clickElement elem

goto' :: forall page. HasFrame page => URL -> page -> Aff Unit
goto' url page = toAffE $ runFn3 _goto_options url page { waitUntil: "domcontentloaded" }

findByText :: forall page. HasFrame page => String -> String -> page -> Aff (Array ElementHandle)
findByText tag selector frame = xpathEval xpath frame
  where xpath = "//" <> tag <> "[text() = '" <> selector <> "']"

type_ :: forall page. HasFrame page => Selector -> String -> page -> Aff Unit
type_ selector text page = Chrome.keyboardType selector text { delay: 10.0 } page

typeDelete_ :: Selector -> Int -> Page -> Aff Unit
typeDelete_ selector n page = do
  Chrome.focus selector page
  sequence_ $ replicate n $ (Chrome.keyboardPress (Chrome.KeyboardKey "Backspace") { delay: 10 } page)

getContent :: Selector -> Page -> Aff String
getContent selector page = do
  theTitleF <- Chrome.unsafePageEval selector "e => e.textContent" page
  pure (unsafeFromForeign theTitleF)

-- Ok for testing I suppose but watch out for the obvious injection
-- attack.
getData :: Selector -> String -> Page -> Aff String
getData selector field page = do
  unsafeFromForeign <$> Chrome.unsafePageEval selector ("e => e.dataset." <> field) page

getHref :: Selector -> Page -> Aff String
getHref selector page = do
  unsafeFromForeign <$> Chrome.unsafePageEval selector "e => e.attributes.href.value" page

getWidth :: Selector -> Page -> Aff Int
getWidth selector page = do
  unsafeFromForeign <$> Chrome.unsafePageEval selector "e => {e.style.display='block';return e.offsetWidth;}" page

-- Note: Any quotes have to be with ' delimiters
countElements :: Selector -> Selector -> Page -> Aff Int
countElements selector (Selector sub) page =
  unsafeFromForeign <$> Chrome.unsafePageEval selector
  ("e => e.querySelectorAll(\"" <> sub <> "\").length") page

assertContent :: Selector -> String -> Page -> Aff Unit
assertContent selector expected page = do
  -- here we wait because most likely we're trying to do this on a new page load
  Aff.delay (Milliseconds 300.0)
  waitFor_ selector page
  content <- getContent selector page
  Assert.equal expected content

assertNotFound :: Selector -> Page -> Aff Unit
assertNotFound selector@(Selector sel) page =
  Aff.attempt (Chrome.unsafePageEval selector "e => e" page) >>= \res -> case res of
    Right _ -> Test.failure $ "selector was expected to not match `" <> sel <> "`"
    Left err -> do
      -- I don't know why this gets duplicated.  This check will need
      -- to be updated if this changes.
      let expected = "Error: Error: failed to find element matching selector"
      if String.take (String.length expected) (show err) == expected then pure unit else liftEffect $ throwException err

back :: Page -> Aff Unit
back page = do
  _ <- Chrome.unsafeEvaluateStringFunction "window.history.back();" page
  pure unit
