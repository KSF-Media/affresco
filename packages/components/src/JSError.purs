module KSF.JSError where

import Effect.Exception (Error)

foreign import packageError :: String -> Error
foreign import orderError   :: String -> Error
