module KSF.Window (close, clearOpener) where

import Prelude

import Effect (Effect)
import Web.HTML.Window (Window)

foreign import close :: Window -> Effect Unit
foreign import clearOpener :: Window -> Effect Unit
