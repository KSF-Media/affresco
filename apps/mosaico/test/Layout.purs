module Mosaico.Test.Layout where

import Prelude

import Mosaico.Test (Test, site)
import KSF.Puppeteer as Chrome
import Test.Unit.Assert as Assert

testLayout :: Test
testLayout page = do
  -- This renders with full screen width if CSS has not loaded
  -- properly
  let menu = Chrome.Selector ".mosaico-header__icon-button--search"
  Chrome.goto (Chrome.URL site) page
  Chrome.waitFor_ menu page
  width <- Chrome.getWidth menu page
  Assert.assert ("Menu width is not too wide " <> show width) $ width < 200
