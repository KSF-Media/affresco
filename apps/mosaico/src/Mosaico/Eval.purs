module Mosaico.Eval where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import evalExternalScriptsImpl :: EffectFn1 (Array String) Unit

evalExternalScripts :: Array String -> Effect Unit
evalExternalScripts = runEffectFn1 evalExternalScriptsImpl
