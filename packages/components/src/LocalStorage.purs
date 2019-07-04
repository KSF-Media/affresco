module KSF.LocalStorage where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import setItem_ :: EffectFn2 String String Unit
foreign import getItem_ :: EffectFn1 String (Nullable String)
foreign import removeItem_ :: EffectFn1 String Unit

setItem :: String -> String -> Effect Unit
setItem = runEffectFn2 setItem_

getItem :: String -> Effect (Maybe String)
getItem key = map toMaybe $ runEffectFn1 getItem_ key

removeItem :: String -> Effect Unit
removeItem = runEffectFn1 removeItem_
