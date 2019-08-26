module KSF.User.User where

import Prelude

import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Parallel (parSequence_)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Class.Console as Console
import Effect.Class.Console as Log
import Effect.Exception (Error, error, throw)
import Effect.Uncurried (mkEffectFn1)
import Facebook.Sdk as FB
import KSF.JanrainSSO as JanrainSSO
import KSF.LocalStorage as LocalStorage
import KSF.Login.Google as Google
import KSF.User.Login.Facebook.Success as Facebook.Success
import Persona (UUID(..))
import Persona as Persona
import React.Basic (JSX)
import React.Basic as React
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import facebookAppId :: String

type Props =
  { onMerge :: Effect Unit
  , onMergeCancelled :: Effect Unit
  , onRegister :: Effect Unit
  , onRegisterCancelled :: Effect Unit
-- TODO:
--  , onLogin :: Either Error Persona.LoginResponse -> Effect Unit
  , onUserFetch :: Either Error Persona.User -> Effect Unit
  , launchAff_ :: Aff Unit -> Effect Unit
  }

type User =
  { logout :: Effect Unit
  , user :: Persona.User
  }

data LoginError e =
  InvalidCredentials
  | FacebookEmailMissing
  | EmailMismatchError
  | GoogleAuthInitError
  | SomethingWentWrong
  | UnexpectedError e

component :: React.Component Props
component = React.createComponent "User"

createUser :: Persona.NewUser -> Aff Persona.LoginResponse
createUser = Persona.register

loginTraditional :: Persona.LoginData -> Aff User
loginTraditional loginData = do
  loginResponse <- Persona.login loginData
  user <- finalizeLogin loginResponse
  pure $ { logout: pure unit, user }

loginByToken :: Effect (Either Error (Aff User))
loginByToken = do
  loadedToken <- loadToken
  case loadedToken of
    Just { uuid, token } -> pure $ Right $ getUser uuid token
    Nothing -> pure $ Left $ error "Did not find token from local storage."

loginSso :: Effect (Either Error (Aff Unit))
loginSso = do
  config <- JanrainSSO.loadConfig
  case Nullable.toMaybe config of
    Nothing -> do
      Console.log "sso_lite.js script is not loaded, giving up"
      pure $ Left $ error "nope"
    Just conf -> pure $ Right $ liftEffect $ checkSsoSession conf
  where
    checkSsoSession loginConfig = do
      JanrainSSO.checkSession $ Record.merge
        loginConfig
        { callback_failure: mkEffectFn1 \a -> do
             Console.log "Janrain SSO failure"
        , callback_success: mkEffectFn1 \a -> do
             Console.log "Janrain SSO success"
             JanrainSSO.setSsoSuccess
        , capture_error: mkEffectFn1 \a -> do
            Console.log "Janrain SSO capture error"
        , capture_success: mkEffectFn1 \r@({ result: { accessToken, userData: { uuid } } }) -> do
             JanrainSSO.setSsoSuccess
             Console.log "Janrain SSO capture success"
             Console.log $ unsafeCoerce r
             Aff.launchAff_ $ finalizeLogin =<< Persona.loginSso { accessToken, uuid }
            }

-- | JS-compatible version of 'logout', takes a callback
--   that will be called when it's done.
jsLogout :: Effect Unit -> Effect Unit
jsLogout callback = Aff.runAff_ (\_ -> callback) logout

-- | Logout the user. Calls social-media SDKs and SSO library.
--   Wipes out local storage.
logout :: Aff Unit
logout = do
  -- use authentication data from local storage to logout first from Persona
  logoutPersona `catchError` Console.errorShow
  -- then we wipe the local storage
  liftEffect deleteToken `catchError` Console.errorShow
  -- then, in parallel, we run all the third-party logouts
  parSequence_
    [ logoutFacebook `catchError` Console.errorShow
    , logoutGoogle   `catchError` Console.errorShow
    , logoutJanrain  `catchError` Console.errorShow
    ]

logoutPersona :: Aff Unit
logoutPersona = do
  token <- liftEffect loadToken
  case token of
    Just t  -> Persona.logout t.uuid t.token
    Nothing -> pure unit

facebookSdk :: Aff FB.Sdk
facebookSdk = FB.init $ FB.defaultConfig facebookAppId

logoutFacebook :: Aff Unit
logoutFacebook = do
  needsFacebookLogout <- liftEffect do
    Facebook.Success.getFacebookSuccess <* Facebook.Success.unsetFacebookSuccess
  when needsFacebookLogout do
    sdk <- facebookSdk
    FB.StatusInfo { status } <- FB.loginStatus sdk
    when (status == FB.Connected) do
      _ <- FB.logout sdk
      Log.info "Logged out from Facebook."

logoutGoogle :: Aff Unit
logoutGoogle = do
  isSignedWithGoogle <- liftEffect Google.isSignedIn
  when isSignedWithGoogle do
    Google.signOut
    Log.info "Logged out from Google."

logoutJanrain :: Aff Unit
logoutJanrain = do
  needsSsoLogout <- liftEffect do
    JanrainSSO.getSsoSuccess <* JanrainSSO.unsetSsoSuccess
  when needsSsoLogout do
    -- If JanrainSSO.checkSession is not called before this function,
    -- the JanrainSSO.endSession will hang.
    -- So call JanrainSSO.checkSession first just to be safe.
    config <- liftEffect $ JanrainSSO.loadConfig
    for_ (Nullable.toMaybe config) \conf -> do
      liftEffect $ JanrainSSO.checkSession conf
      JanrainSSO.endSession conf

getUser :: UUID -> Persona.Token -> Aff User
getUser uuid token = do
  userResponse <- try do
    Persona.getUser uuid token
  case userResponse of
    Left err
      | Just (errData :: Persona.TokenInvalid) <- Persona.errorData err -> do
          Console.error "Failed to fetch the user: Invalid token"
          liftEffect deleteToken
          throwError err
      | otherwise -> do
          Console.error "Failed to fetch the user"
          throwError err
    Right user -> do
      Console.info "User fetched successfully"
      pure { user, logout: pure unit }

finalizeLogin :: Persona.LoginResponse -> Aff Persona.User
finalizeLogin loginResponse = do
  saveToken loginResponse
  userResponse <- try do
    Persona.getUser loginResponse.uuid loginResponse.token
  case userResponse of
    Left err
      | Just (errData :: Persona.TokenInvalid) <- Persona.errorData err -> do
          Console.error "Failed to fetch the user: Invalid token"
          liftEffect deleteToken
          throwError err
      | otherwise -> do
          Console.error "Failed to fetch the user"
          throwError err
    Right user -> do
      Console.info "User fetched successfully"
      pure user
 -- liftEffect $ props.onUserFetch userRespons

loadToken :: forall m. MonadEffect m => m (Maybe Persona.LoginResponse)
loadToken = liftEffect $ runMaybeT do
  token <- map Persona.Token $ MaybeT $ LocalStorage.getItem "token"
  uuid <- map Persona.UUID $ MaybeT $ LocalStorage.getItem "uuid"
  pure { token, ssoCode: Nullable.toNullable Nothing, uuid }

saveToken :: forall m. MonadEffect m => Persona.LoginResponse -> m Unit
saveToken { token, ssoCode, uuid } = liftEffect do
  for_ (Nullable.toMaybe ssoCode) $ \code -> do
    config <- JanrainSSO.loadConfig
    for_ (Nullable.toMaybe config) \conf -> JanrainSSO.setSession conf code
  LocalStorage.setItem "token" case token of Persona.Token a -> a
  LocalStorage.setItem "uuid" case uuid of Persona.UUID a -> a

deleteToken :: Effect Unit
deleteToken = traverse_ LocalStorage.removeItem [ "token", "uuid" ]

requireToken :: forall m. MonadEffect m => m Persona.LoginResponse
requireToken =
  loadToken >>= case _ of
    Nothing -> liftEffect $ throw "Did not find uuid/token in local storage."
    Just loginResponse -> pure loginResponse
