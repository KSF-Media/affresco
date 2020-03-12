module KSF.JSError where

import Effect.Exception (Error)

foreign import packageError :: String -> Error
foreign import orderError   :: String -> Error
foreign import loginError   :: String -> Error
