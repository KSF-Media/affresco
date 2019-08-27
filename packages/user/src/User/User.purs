module KSF.User.User
  ( UserError (..)
  , MergeInfo
  , ValidationServerError
  , module PersonaReExport
  , loginTraditional
  , magicLogin
  , logout
  , someAuth
  , facebookSdk
  , createUser
  , updateUser
  , pauseSubscription
  , temporaryAddressChange
  )
where

import Prelude

import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Parallel (parSequence_)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Class.Console as Log
import Effect.Exception (Error, throw)
import Effect.Uncurried (mkEffectFn1)
import Facebook.Sdk as FB
import Foreign.Object (Object)
import KSF.JanrainSSO as JanrainSSO
import KSF.LocalStorage as LocalStorage
import KSF.User.Login.Facebook.Success as Facebook.Success
import KSF.User.Login.Google as Google
import Persona (User, MergeToken, Provider(..), UUID, Email(..), Token(..), InvalidPauseDateError(..), InvalidDateInput(..), UserUpdate(..)) as PersonaReExport
import Persona as Persona
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import facebookAppId :: String

data UserError =
  LoginInvalidCredentials
  | LoginFacebookEmailMissing
  | LoginEmailMismatchError
  | LoginGoogleAuthInitError
  | LoginTokenInvalid
  | InvalidFormFields ValidationServerError
  | RegistrationEmailInUse
  | MergeEmailInUse MergeInfo
  | SomethingWentWrong
  | UnexpectedError Error

type ValidationServerError = Object (Array String)

type MergeInfo =
  { token :: Persona.MergeToken
  , existingProvider :: Persona.Provider
  , newProvider :: Persona.Provider
  , userEmail :: Persona.Email
  }

createUser :: Persona.NewUser -> Aff (Either UserError Persona.User)
createUser newUser = do
  registeredUser <- try $ Persona.register newUser
  case registeredUser of
    Left err
      | Just (errData :: Persona.EmailAddressInUseRegistration) <- Persona.errorData err -> do
          Console.error errData.email_address_in_use_registration.description
          pure $ Left RegistrationEmailInUse
      | Just (errData :: Persona.InvalidFormFields) <- Persona.errorData err -> do
          Console.error errData.invalid_form_fields.description
          pure $ Left $ InvalidFormFields errData.invalid_form_fields.errors
      | otherwise -> do
          Console.error "An unexpected error occurred during registration"
          pure $ Left $ UnexpectedError err
    Right user -> finalizeLogin user

getUser :: Persona.UUID -> Persona.Token -> Aff Persona.User
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
      pure user

updateUser :: Persona.UUID -> Persona.UserUpdate -> Aff (Either UserError Persona.User)
updateUser uuid update = do
  newUser <- try $ Persona.updateUser uuid update <<< _.token =<< requireToken
  case newUser of
    Right user -> pure $ Right user
    Left err   -> pure $ Left $ UnexpectedError err

loginTraditional :: Persona.LoginData -> Aff (Either UserError Persona.User)
loginTraditional loginData = do
  loginResponse <- try $ Persona.login loginData
  case loginResponse of
    Right lr -> finalizeLogin lr
    Left err
      | Just (errData :: Persona.InvalidCredentials) <- Persona.errorData err -> do
          Console.error errData.invalid_credentials.description
          pure $ Left LoginInvalidCredentials
      | Just serverError <- Persona.internalServerError err -> do
          Console.error "Something went wrong with traditional login"
          pure $ Left SomethingWentWrong
      | otherwise -> do
          Console.error "An unexpected error occurred during traditional login"
          pure $ Left $ UnexpectedError err

-- | Tries to login with token in local storage or, if that fails, SSO.
magicLogin :: (Either UserError Persona.User -> Effect Unit) -> Aff Unit
magicLogin callback = do
  loadedToken <- loadToken
  case loadedToken of
    Just token -> do
      Console.log "Successfully loaded the saved token from local storage"
      user <- finalizeLogin token
      liftEffect $ callback user
    Nothing -> do
      Console.log "Couldn't load the saved token, giving SSO a try"
      loginSso callback `catchError` case _ of
        err | Just serverError <- Persona.internalServerError err -> do
                Console.error "Something went wrong with SSO login"
                liftEffect $ callback $ Left SomethingWentWrong
                throwError err
            | otherwise -> do
                Console.error "An unexpected error occurred during SSO login"
                liftEffect $ callback $ Left $ UnexpectedError err
                throwError err

someAuth
  :: Maybe MergeInfo
  -> Persona.Email
  -> Persona.Token
  -> Persona.Provider
  -> Aff (Either UserError Persona.User)
someAuth mergeInfo email token provider = do
  loginResponse <- try $ Persona.loginSome
      { provider: show provider
      , someToken: token
      , mergeToken: toNullable $ map _.token mergeInfo
      }
  case loginResponse of
    Right t -> finalizeLogin t
    Left err
      | Just (errData :: Persona.EmailAddressInUse) <- Persona.errorData err -> do
          Console.error errData.email_address_in_use.description
          pure $ Left $ MergeEmailInUse newMergeInfo
          where
            newMergeInfo =
              { token: errData.email_address_in_use.merge_token
              , existingProvider: errData.email_address_in_use.existing_provider
              , newProvider: provider
              , userEmail: email
              }
       | Just serverError <- Persona.internalServerError err -> do
           Console.error "Something went wrong with SoMe login"
           pure $ Left SomethingWentWrong
       | otherwise -> do
           Console.error "An unexpected error occurred during SoMe login"
           pure $ Left $ UnexpectedError err

loginSso :: (Either UserError Persona.User -> Effect Unit) -> Aff Unit
loginSso callback = do
  config <- liftEffect $ JanrainSSO.loadConfig
  case Nullable.toMaybe config of
    Nothing -> do
      Console.log "sso_lite.js script is not loaded, giving up"
      pure unit
    Just conf -> liftEffect $ checkSsoSession conf
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
             Aff.launchAff_ do
               loginResponse <-
                 Persona.loginSso { accessToken, uuid } `catchError` case _ of
                      err | Just serverError <- Persona.internalServerError err -> do
                              Console.error "Something went wrong with SSO login"
                              liftEffect $ callback $ Left SomethingWentWrong
                              throwError err
                          | otherwise -> do
                              Console.error "An unexpected error occurred during SSO login"
                              liftEffect $ callback $ Left $ UnexpectedError err
                              throwError err
               user <- finalizeLogin loginResponse
               liftEffect $ callback user
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

finalizeLogin :: Persona.LoginResponse -> Aff (Either UserError Persona.User)
finalizeLogin loginResponse = do
  saveToken loginResponse
  userResponse <- try do
    Persona.getUser loginResponse.uuid loginResponse.token
  case userResponse of
    Left err
      | Just (errData :: Persona.TokenInvalid) <- Persona.errorData err -> do
          Console.error "Failed to fetch the user: Invalid token"
          liftEffect deleteToken
          pure $ Left LoginTokenInvalid
      | otherwise -> do
          Console.error "Failed to fetch the user"
          pure $ Left $ UnexpectedError err
    Right user -> do
      Console.info "User fetched successfully"
      pure $ Right user

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

jsUpdateGdprConsent
  :: Persona.UUID
  -> Persona.Token
  -> Array Persona.GdprConsent
  -> Effect Unit
  -> Effect Unit
jsUpdateGdprConsent uuid token consents callback
  = Aff.runAff_ (\_ -> callback) $ Persona.updateGdprConsent uuid token consents

facebookSdk :: Aff FB.Sdk
facebookSdk = FB.init $ FB.defaultConfig facebookAppId

pauseSubscription
  :: Persona.UUID
  -> Int
  -> DateTime
  -> DateTime
  -> Aff (Either Persona.InvalidDateInput Persona.Subscription)
pauseSubscription userUuid subsno startDate endDate = do
  pausedSub <- try $ Persona.pauseSubscription userUuid subsno startDate endDate <<< _.token =<< requireToken
  case pausedSub of
    Right sub -> pure $ Right sub
    Left err
      | Just (errData :: Persona.InvalidPauseDates) <- Persona.errorData err ->
          pure $ Left $ Persona.pauseDateErrorToInvalidDateError errData.invalid_pause_dates.message
      | otherwise -> do
          Console.error "Unexpected error when pausing subscription."
          pure $ Left $ Persona.pauseDateErrorToInvalidDateError Persona.PauseInvalidUnexpected

temporaryAddressChange
  :: Persona.UUID
  -> Int
  -> DateTime
  -> DateTime
  -> String
  -> String
  -> Aff (Either Persona.InvalidDateInput Persona.Subscription)
temporaryAddressChange userUuid subsno startDate endDate streetAddress zipCode = do
  addressChangedSub <- try $ Persona.temporaryAddressChange userUuid subsno startDate endDate streetAddress zipCode <<< _.token =<< requireToken
  case addressChangedSub of
    Right sub -> pure $ Right sub
    Left err
      | Just (errData :: Persona.InvalidDates) <- Persona.errorData err ->
          pure $ Left errData.invalid_param.message
      | otherwise -> do
          Console.error "Unexpected error when making temporary address change."
          pure $ Left Persona.InvalidUnexpected
