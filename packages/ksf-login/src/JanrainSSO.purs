module JanrainSSO where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Uncurried (EffectFn1, runEffectFn1)
import LocalStorage as LocalStorage

foreign import loadConfig :: Effect Config

setSsoSuccess :: Effect Unit
setSsoSuccess = LocalStorage.setItem "KSF_JANRAIN_SSO_SUCCESS" "true"

unsetSsoSuccess :: Effect Unit
unsetSsoSuccess = LocalStorage.removeItem "KSF_JANRAIN_SSO_SUCCESS"

getSsoSuccess :: Effect Boolean
getSsoSuccess = isJust <$> LocalStorage.getItem "KSF_JANRAIN_SSO_SUCCESS"

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

endSession :: Aff Unit
endSession = Aff.makeAff \callback -> do
  Exception.catchException
    (\err -> do
      Console.error $ "JanrainSSO.end_session failed: " <> Exception.message err
      callback $ Left err)
    (runEffectFn1 sso.end_session $ Nullable.toNullable $ Just $ callback $ Right unit)
  pure Aff.nonCanceler

type Config =
  { client_id    :: String
  , flow_name    :: String
  , flow_version :: String
  , locale       :: String
  , redirect_uri :: String
  , sso_server   :: String
  , xd_receiver  :: String
  }
