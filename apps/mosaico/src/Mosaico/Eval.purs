module Mosaico.Eval where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import evalExternalScriptsImpl :: EffectFn1 (Array String) Unit

evalExternalScripts :: Array ScriptTag -> Effect Unit
evalExternalScripts scriptTags = runEffectFn1 evalExternalScriptsImpl $ map (\(ScriptTag scriptTag) -> scriptTag) scriptTags

-- This is supposed to be a complete script tag
-- <script></script>
newtype ScriptTag = ScriptTag String
