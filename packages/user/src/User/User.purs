module KSF.User
  ( UserError (..)
  , MergeInfo
  , ValidationServerError
  , module PersonaReExport
  , module BottegaReExport
  , loginTraditional
  , magicLogin
  , logout
  , someAuth
  , facebookSdk
  , createUser
  , createUserWithEmail
  , updateUser
  , updatePassword
  , pauseSubscription
  , unpauseSubscription
  , temporaryAddressChange
  , createDeliveryReclamation
  , createOrder
  , payOrder
  , getOrder
  , getPackages
  , module Api
  , module Subscription
  )
where

import Prelude

import Bottega (NewOrder, Order, OrderNumber, OrderStatusFailReason(..), OrderStatusState(..), PaymentMethod(..), PaymentTerminalUrl) as BottegaReExport
import Bottega as Bottega
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Parallel (parSequence_)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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
import Effect.Exception as Error
import Effect.Uncurried (mkEffectFn1)
import Facebook.Sdk as FB
import Foreign.Object (Object)
import KSF.Api (InvalidateCache)
import KSF.Api (Token(..), UUID(..), UserAuth, oauthToken, Password) as Api
import KSF.Api.Package (Package)
import KSF.Api.Subscription (DeliveryAddress, PendingAddressChange, SubscriptionState(..), Subscription, PausedSubscription, SubscriptionDates) as Subscription
import KSF.Error as KSF.Error
import KSF.JanrainSSO as JanrainSSO
import KSF.LocalStorage as LocalStorage
import KSF.User.Login.Facebook.Success as Facebook.Success
import KSF.User.Login.Google as Google
import Persona (User, MergeToken, Provider(..), Email(..), InvalidPauseDateError(..), InvalidDateInput(..), UserUpdate(..), Address, DeliveryReclamation, DeliveryReclamationClaim, NewTemporaryUser) as PersonaReExport
import Persona as Persona
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import facebookAppId :: String

data UserError =
  LoginInvalidCredentials
  | LoginFacebookEmailMissing
  | LoginEmailMismatchError
  | LoginGoogleAuthInitError
  | LoginGoogleAuthInitErrorOrigin
  | LoginTokenInvalid
  | InvalidFormFields ValidationServerError
  | RegistrationEmailInUse
  | MergeEmailInUse MergeInfo
  | SomethingWentWrong
  | UnexpectedError Error
derive instance genericUserError :: Generic UserError _
instance showUserError :: Show UserError where
  show = genericShow

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
    Right user -> finalizeLogin Nothing user

createUserWithEmail :: Persona.NewTemporaryUser -> Aff (Either UserError Persona.User)
createUserWithEmail newTemporaryUser = do
  newUser <- try $ Persona.registerWithEmail newTemporaryUser
  case newUser of
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
    Right user -> finalizeLogin Nothing user


getUser :: Maybe InvalidateCache -> Api.UUID -> Api.Token -> Aff Persona.User
getUser maybeInvalidateCache uuid token = do
  userResponse <- try do
    Persona.getUser maybeInvalidateCache uuid token
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

updateUser :: Api.UUID -> Persona.UserUpdate -> Aff (Either UserError Persona.User)
updateUser uuid update = do
  newUser <- try $ Persona.updateUser uuid update <<< _.token =<< requireToken
  case newUser of
    Right user -> pure $ Right user
    Left err   -> pure $ Left $ UnexpectedError err

updatePassword :: Api.UUID -> Api.Password -> Api.Password -> Aff (Either UserError Persona.User)
updatePassword uuid password confirmPassword = do
  eitherUser <- try $ Persona.updatePassword uuid password confirmPassword <<< _.token =<< requireToken
  case eitherUser of
    Left err   -> pure $ Left $ UnexpectedError err
    Right user -> pure $ Right user

loginTraditional :: Persona.LoginData -> Aff (Either UserError Persona.User)
loginTraditional loginData = do
  loginResponse <- try $ Persona.login loginData
  case loginResponse of
    Right lr -> finalizeLogin Nothing lr
    Left err
      | Just (errData :: Persona.InvalidCredentials) <- Persona.errorData err -> do
          Console.error errData.invalid_credentials.description
          pure $ Left LoginInvalidCredentials
      | Just serverError <- KSF.Error.internalServerError err -> do
          Console.error "Something went wrong with traditional login"
          pure $ Left SomethingWentWrong
      | otherwise -> do
          Console.error "An unexpected error occurred during traditional login"
          pure $ Left $ UnexpectedError err

-- | Tries to login with token in local storage or, if that fails, SSO.
magicLogin :: Maybe InvalidateCache -> (Either UserError Persona.User -> Effect Unit) -> Aff Unit
magicLogin maybeInvalidateCache callback = do
  loadedToken <- loadToken
  case loadedToken of
    Just token -> do
      Console.log "Successfully loaded the saved token from local storage"
      user <- finalizeLogin maybeInvalidateCache token
      liftEffect $ callback user
    Nothing -> do
      Console.log "Couldn't load the saved token, giving SSO a try"
      loginSso maybeInvalidateCache callback `catchError` case _ of
        err | Just serverError <- KSF.Error.internalServerError err -> do
                Console.error "Something went wrong with SSO login"
                liftEffect $ callback $ Left SomethingWentWrong
                throwError err
            | otherwise -> do
                Console.error "An unexpected error occurred during SSO login"
                liftEffect $ callback $ Left $ UnexpectedError err
                throwError err

someAuth
  :: Maybe InvalidateCache
  -> Maybe MergeInfo
  -> Persona.Email
  -> Api.Token
  -> Persona.Provider
  -> Aff (Either UserError Persona.User)
someAuth maybeInvalidateCache mergeInfo email token provider = do
  loginResponse <- try $ Persona.loginSome
      { provider: show provider
      , someToken: token
      , mergeToken: toNullable $ map _.token mergeInfo
      }
  case loginResponse of
    Right t -> finalizeLogin maybeInvalidateCache t
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
      | Just serverError <- KSF.Error.internalServerError err -> do
           Console.error "Something went wrong with SoMe login"
           pure $ Left SomethingWentWrong
      | otherwise -> do
           Console.error "An unexpected error occurred during SoMe login"
           pure $ Left $ UnexpectedError err

loginSso :: Maybe InvalidateCache -> (Either UserError Persona.User -> Effect Unit) -> Aff Unit
loginSso maybeInvalidateCache callback = do
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
                      err | Just serverError <- KSF.Error.internalServerError err -> do
                              -- TODO: What is the desired action here?
                              Console.error "Something went wrong with SSO login"
                              liftEffect $ callback $ Left SomethingWentWrong
                              throwError err
                          | otherwise -> do
                              Console.error "An unexpected error occurred during SSO login"
                              liftEffect $ callback $ Left $ UnexpectedError err
                              throwError err
               user <- finalizeLogin maybeInvalidateCache loginResponse
               liftEffect $ callback user
            }

-- | Logout the user. Calls social-media SDKs and SSO library.
--   Wipes out local storage.
logout :: (Either Error Unit -> Effect Unit) -> Aff Unit
logout handleLogout = do
  -- use authentication data from local storage to logout first from Persona
  -- NOTE: In case this request fails, we still want to clear local storage and continue with the social logouts.
  logoutResponse <- try logoutPersona
  -- then we wipe the local storage
  liftEffect deleteToken `catchError` Console.errorShow
  -- then, in parallel, we run all the third-party logouts
  parSequence_
    [ logoutFacebook `catchError` Console.errorShow
    , logoutGoogle   `catchError` Console.errorShow
    , logoutJanrain  `catchError` Console.errorShow
    ]
  liftEffect $ handleLogout logoutResponse

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

finalizeLogin :: Maybe InvalidateCache -> Persona.LoginResponse -> Aff (Either UserError Persona.User)
finalizeLogin maybeInvalidateCache loginResponse = do
  saveToken loginResponse
  userResponse <- try do
    Persona.getUser maybeInvalidateCache loginResponse.uuid loginResponse.token
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
  token <- map Api.Token $ MaybeT $ LocalStorage.getItem "token"
  uuid <- map Api.UUID $ MaybeT $ LocalStorage.getItem "uuid"
  pure { token, ssoCode: Nullable.toNullable Nothing, uuid }

saveToken :: forall m. MonadEffect m => Persona.LoginResponse -> m Unit
saveToken { token, ssoCode, uuid } = liftEffect do
  for_ (Nullable.toMaybe ssoCode) $ \code -> do
    config <- JanrainSSO.loadConfig
    for_ (Nullable.toMaybe config) \conf -> JanrainSSO.setSession conf code
  LocalStorage.setItem "token" case token of Api.Token a -> a
  LocalStorage.setItem "uuid" case uuid of Api.UUID a -> a

deleteToken :: Effect Unit
deleteToken = traverse_ LocalStorage.removeItem [ "token", "uuid" ]

requireToken :: forall m. MonadEffect m => m Persona.LoginResponse
requireToken =
  loadToken >>= case _ of
    Nothing -> liftEffect $ throw "Did not find uuid/token in local storage."
    Just loginResponse -> pure loginResponse

jsUpdateGdprConsent
  :: Api.UUID
  -> Api.Token
  -> Array Persona.GdprConsent
  -> Effect Unit
  -> Effect Unit
jsUpdateGdprConsent uuid token consents callback
  = Aff.runAff_ (\_ -> callback) $ Persona.updateGdprConsent uuid token consents

facebookSdk :: Aff FB.Sdk
facebookSdk = FB.init $ FB.defaultConfig facebookAppId

pauseSubscription
  :: Api.UUID
  -> Int
  -> DateTime
  -> DateTime
  -> Aff (Either Persona.InvalidDateInput Subscription.Subscription)
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

unpauseSubscription
  :: Api.UUID
  -> Int
  -> Maybe DateTime
  -> Aff Subscription.Subscription
unpauseSubscription userUuid subsno startDate = do
  Persona.unpauseSubscription userUuid subsno startDate <<< _.token =<< requireToken

temporaryAddressChange
  :: Api.UUID
  -> Int
  -> DateTime
  -> DateTime
  -> String
  -> String
  -> String
  -> Maybe String
  -> Aff (Either Persona.InvalidDateInput Subscription.Subscription)
temporaryAddressChange userUuid subsno startDate endDate streetAddress zipCode countryCode temporaryName = do
  addressChangedSub <- try $ Persona.temporaryAddressChange userUuid subsno startDate endDate streetAddress zipCode countryCode temporaryName <<< _.token =<< requireToken
  case addressChangedSub of
    Right sub -> pure $ Right sub
    Left err
      | Just (errData :: Persona.InvalidDates) <- Persona.errorData err ->
          pure $ Left errData.invalid_param.message
      | otherwise -> do
          Console.error "Unexpected error when making temporary address change."
          pure $ Left Persona.InvalidUnexpected

createDeliveryReclamation
  :: Api.UUID
  -> Int
  -> DateTime
  -> PersonaReExport.DeliveryReclamationClaim
  -> Aff (Either Persona.InvalidDateInput Persona.DeliveryReclamation)
createDeliveryReclamation uuid subsno date claim = do
  deliveryReclamation <- try $ Persona.createDeliveryReclamation uuid subsno date claim <<< _.token =<< requireToken
  case deliveryReclamation of
    Right recl -> pure $ Right recl
    Left err -> do
      Console.error "Unexpected error when creating delivery reclamation."
      pure $ Left Persona.InvalidUnexpected

createOrder :: Bottega.NewOrder -> Aff (Either String Bottega.Order)
createOrder newOrder = callBottega \tokens -> Bottega.createOrder { userId: tokens.uuid, authToken: tokens.token } newOrder

payOrder :: Bottega.OrderNumber -> Bottega.PaymentMethod -> Aff (Either String Bottega.PaymentTerminalUrl)
payOrder orderNum paymentMethod = callBottega $ \tokens ->  Bottega.payOrder { userId: tokens.uuid, authToken: tokens.token } orderNum paymentMethod

getOrder :: Bottega.OrderNumber -> Aff (Either String Bottega.Order)
getOrder orderNum = callBottega $ \tokens -> Bottega.getOrder { userId: tokens.uuid, authToken: tokens.token } orderNum

callBottega :: forall a. (Persona.LoginResponse -> Aff a) -> Aff (Either String a)
callBottega f = do
  tokens <- requireToken
  (try $ f tokens) >>= case _ of
    Right a  -> pure $ Right a
    -- TODO: Come up with better errors
    Left err -> pure $ Left $ Error.message err

getPackages :: Aff (Array Package)
getPackages = Bottega.getPackages
