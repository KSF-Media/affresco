module JanrainSSO where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import loadConfig :: Effect Config

foreign import sso ::
  { check_session
      :: forall params
       . EffectFn1
           { | params } -- ^ Typing? Nah. See https://docs.janrain.com/registration/sso/#parameters-for-check-session-config-object
           Unit
  , set_session :: EffectFn1 String Unit
  , end_session :: EffectFn1 (Nullable (Effect Unit)) Unit
  }

checkSession :: forall params . { | params } -> Effect Unit
checkSession = runEffectFn1 sso.check_session

setSession :: String -> Effect Unit
setSession = runEffectFn1 sso.set_session

endSession :: Maybe (Effect Unit) -> Effect Unit
endSession = runEffectFn1 sso.end_session <<< Nullable.toNullable

type Config =
  { client_id    :: String
  , flow_name    :: String
  , flow_version :: String
  , locale       :: String
  , redirect_uri :: String
  , sso_server   :: String
  , xd_receiver  :: String
  }
