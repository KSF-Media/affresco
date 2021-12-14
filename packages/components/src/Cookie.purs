module KSF.Cookie where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import setCookie_ :: EffectFn2 String String Unit
foreign import getValue_ :: EffectFn1 String (Nullable String)
foreign import deleteCookie_ :: EffectFn1 String Unit

setCookie :: String -> String -> Effect Unit
setCookie = runEffectFn2 setCookie_

getValue :: String -> Effect (Maybe String)
getValue = map toMaybe <<< runEffectFn1 getValue_

deleteCookie :: String -> Effect Unit
deleteCookie = runEffectFn1 deleteCookie_
