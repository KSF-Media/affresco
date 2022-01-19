module Mosaico.Analytics where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import _pushToDataLayer :: EffectFn2 String String Unit
pushToDataLayer :: String -> String -> Effect Unit
pushToDataLayer = runEffectFn2 _pushToDataLayer