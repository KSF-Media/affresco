module OpenApiClient where

import Data.Function.Uncurried (Fn4, runFn4)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign)

foreign import data Api :: Type

type MethodName = String

foreign import callApi_
  :: forall req res opts
   . Fn4
       Api
       MethodName
       req
       { | opts }
       (EffectFnAff res)

callApi :: forall res opts. Api -> MethodName -> Array Foreign -> { | opts } -> Aff res
callApi api methodName req opts =
  fromEffectFnAff (runFn4 callApi_ api methodName req opts)
