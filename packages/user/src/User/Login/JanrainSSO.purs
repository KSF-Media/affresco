module KSF.JanrainSSO where

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
import KSF.LocalStorage as LocalStorage
import Unsafe.Coerce (unsafeCoerce)

foreign import loadConfig :: Effect (Nullable (Config ()))

setSsoSuccess :: Effect Unit
setSsoSuccess = do
  LocalStorage.setItem "KSF_JANRAIN_SSO_SUCCESS" "true"
  Console.log "Set KSF_JANRAIN_SSO_SUCCESS"

unsetSsoSuccess :: Effect Unit
unsetSsoSuccess = do
  LocalStorage.removeItem "KSF_JANRAIN_SSO_SUCCESS"
  Console.log "Unset KSF_JANRAIN_SSO_SUCCESS"

getSsoSuccess :: Effect Boolean
getSsoSuccess = do
  item <- LocalStorage.getItem "KSF_JANRAIN_SSO_SUCCESS"
  Console.log $ "KSF_JANRAIN_SSO_SUCCESS: " <> show item
  pure $ isJust item

foreign import sso ::
  { check_session
      :: forall params
       . EffectFn1
           { | params } -- ^ Typing? Nah. See https://docs.janrain.com/registration/sso/#parameters-for-check-session-config-object
           Unit
  , set_session :: EffectFn1 String Unit
  , end_session :: EffectFn1 (Nullable (Effect Unit)) Unit
  }

checkSession :: forall params. Config params -> Effect Unit
checkSession config = do
  Console.log "Calling SSO.check_session"
  Console.log $ unsafeCoerce config
  runEffectFn1 sso.check_session config

setSession :: forall params. Config params -> String -> Effect Unit
setSession _config session = do
  Console.log $ "Calling SSO.set_session " <> session
  runEffectFn1 sso.set_session session

endSession :: forall params. Config params -> Aff Unit
endSession _config = Aff.makeAff \callback -> do
  Exception.catchException
    (\err -> do
      Console.error $ "JanrainSSO.end_session failed: " <> Exception.message err
      callback $ Left err)
    (do Console.log "Calling SSO.end_session"
        runEffectFn1 sso.end_session $ Nullable.toNullable $ Just $ do
          Console.log "SSO.end_session called back"
          callback $ Right unit)
  pure Aff.nonCanceler

type Config params =
  { client_id    :: String
  , flow_name    :: String
  , flow_version :: String
  , locale       :: String
  , redirect_uri :: String
  , sso_server   :: String
  , xd_receiver  :: String
  | params
  }
