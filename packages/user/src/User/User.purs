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
  , getUser
  , isAdminUser
  , updateUser
  , updatePassword
  , pauseSubscription
  , unpauseSubscription
  , temporaryAddressChange
  , deleteTemporaryAddressChange
  , createDeliveryReclamation
  , searchUsers
  , getPayments
  , createOrder
  , payOrder
  , getOrder
  , getCreditCards
  , getCreditCard
  , deleteCreditCard
  , registerCreditCard
  , getCreditCardRegister
  , updateCreditCardSubscriptions
  , getPackages
  , getUserEntitlementsLoadToken
  , module Api
  , module Subscription
  )
where

import Prelude

import Bottega (BottegaError(..))
import Bottega (createOrder, getOrder, getPackages, payOrder, getCreditCards, getCreditCard, deleteCreditCard, registerCreditCard, getCreditCardRegister, updateCreditCardSubscriptions, InsufficientAccount) as Bottega
import Bottega.Models (NewOrder, Order, OrderNumber, OrderState(..), FailReason(..), PaymentMethod(..), PaymentTerminalUrl) as BottegaReExport
import Bottega.Models (NewOrder, Order, OrderNumber, PaymentTerminalUrl, CreditCardId, CreditCard, CreditCardRegisterNumber, CreditCardRegister) as Bottega
import Bottega.Models.PaymentMethod (PaymentMethod) as Bottega
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
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Class.Console as Log
import Effect.Exception (Error, error, throw)
import Effect.Exception as Error
import Effect.Uncurried (mkEffectFn1)
import Facebook.Sdk as FB
import Foreign.Object (Object)
import KSF.Api (InvalidateCache, UserAuth)
import KSF.Api (Token(..), UUID(..), UserAuth, oauthToken, Password) as Api
import KSF.Api.Error as Api.Error
import KSF.Api.Package (Package)
import KSF.Api.Subscription (DeliveryAddress, PendingAddressChange, SubscriptionState(..), Subscription, PausedSubscription, SubscriptionDates) as Subscription
import KSF.Error as KSF.Error
import KSF.JanrainSSO as JanrainSSO
import KSF.LocalStorage as LocalStorage
import KSF.User.Login.Facebook.Success as Facebook.Success
import KSF.User.Login.Google as Google
import Persona (User, MergeToken, Provider(..), Email(..), InvalidPauseDateError(..), InvalidDateInput(..), UserUpdate(..), Address, DeliveryReclamation, DeliveryReclamationClaim, NewTemporaryUser, SubscriptionPayments, Payment, PaymentType(..), PaymentState(..)) as PersonaReExport
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
  | ServiceUnavailable
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
      | Just (errData :: Persona.EmailAddressInUseRegistration) <- Api.Error.errorData err -> do
          Console.error errData.email_address_in_use_registration.description
          pure $ Left RegistrationEmailInUse
      | Just (errData :: Persona.InvalidFormFields) <- Api.Error.errorData err -> do
          Console.error errData.invalid_form_fields.description
          pure $ Left $ InvalidFormFields errData.invalid_form_fields.errors
      | otherwise -> do
          Console.error "An unexpected error occurred during registration"
          pure $ Left $ UnexpectedError err
    Right user -> finalizeLogin Nothing =<< saveToken user

createUserWithEmail :: Persona.NewTemporaryUser -> Aff (Either UserError Persona.User)
createUserWithEmail newTemporaryUser = do
  newUser <- try $ Persona.registerWithEmail newTemporaryUser
  case newUser of
    Left err
      | Just (errData :: Persona.EmailAddressInUseRegistration) <- Api.Error.errorData err -> do
          Console.error errData.email_address_in_use_registration.description
          pure $ Left RegistrationEmailInUse
      | Just (errData :: Persona.InvalidFormFields) <- Api.Error.errorData err -> do
          Console.error errData.invalid_form_fields.description
          pure $ Left $ InvalidFormFields errData.invalid_form_fields.errors
      | otherwise -> do
          Console.error "An unexpected error occurred during registration"
          pure $ Left $ UnexpectedError err
    Right user -> finalizeLogin Nothing =<< saveToken user


getUser :: Maybe InvalidateCache -> Api.UUID -> Aff Persona.User
getUser maybeInvalidateCache uuid = do
  userResponse <- try do
    Persona.getUser maybeInvalidateCache uuid =<< requireToken
  case userResponse of
    Left err
      | Just (errData :: Persona.TokenInvalid) <- Api.Error.errorData err -> do
          Console.error "Failed to fetch the user: Invalid token"
          liftEffect deleteToken
          throwError err
      | otherwise -> do
          Console.error "Failed to fetch the user"
          throwError err
    Right user -> do
      Console.info "User fetched successfully"
      pure user

isAdminUser :: Effect Boolean
isAdminUser = (_ == Just "1") <$> LocalStorage.getItem "isAdmin"

getUserEntitlementsLoadToken :: Aff (Either UserError (Set String))
getUserEntitlementsLoadToken = do
  tokens <- loadToken
  case tokens of
    Just auth -> getUserEntitlements auth
    _ -> pure $ Left (UnexpectedError $ error "Could not load tokens from local storage")

getUserEntitlements :: UserAuth -> Aff (Either UserError (Set String))
getUserEntitlements auth = do
  eitherEntitlements <- try $ Persona.getUserEntitlements auth
  case eitherEntitlements of
    Right entitlements -> pure $ Right $ Set.fromFoldable entitlements
    Left err
      | Just (_ :: Persona.TokenInvalid) <- Api.Error.errorData err ->
        pure $ Left LoginTokenInvalid
      -- TODO: Handle other errors as well
      | otherwise ->
        pure $ Left $ UnexpectedError err

updateUser :: Api.UUID -> Persona.UserUpdate -> Aff (Either UserError Persona.User)
updateUser uuid update = do
  newUser <- try $ Persona.updateUser uuid update =<< requireToken
  case newUser of
    Right user -> pure $ Right user
    Left err   -> pure $ Left $ UnexpectedError err

updatePassword :: Api.UUID -> Api.Password -> Api.Password -> Aff (Either UserError Persona.User)
updatePassword uuid password confirmPassword = do
  eitherUser <- try $ Persona.updatePassword uuid password confirmPassword =<< requireToken
  case eitherUser of
    Left err   -> pure $ Left $ UnexpectedError err
    Right user -> pure $ Right user

loginTraditional :: Persona.LoginData -> Aff (Either UserError Persona.User)
loginTraditional loginData = do
  loginResponse <- try $ Persona.login loginData
  case loginResponse of
    Right lr -> finalizeLogin Nothing =<< saveToken lr
    Left err
      | Just (errData :: Persona.InvalidCredentials) <- Api.Error.errorData err -> do
          Console.error errData.invalid_credentials.description
          pure $ Left LoginInvalidCredentials
      | KSF.Error.serviceUnavailableError err -> do
          Console.error "Service unavailable with traditional login"
          pure $ Left ServiceUnavailable
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
        err | KSF.Error.serviceUnavailableError err -> do
                Console.error "Service unavailable with SSO login"
                liftEffect $ callback $ Left ServiceUnavailable
                throwError err
            | Just serverError <- KSF.Error.internalServerError err -> do
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
    Right t -> finalizeLogin maybeInvalidateCache =<< saveToken t
    Left err
      | Just (errData :: Persona.EmailAddressInUse) <- Api.Error.errorData err -> do
          Console.error errData.email_address_in_use.description
          pure $ Left $ MergeEmailInUse newMergeInfo
          where
            newMergeInfo =
              { token: errData.email_address_in_use.merge_token
              , existingProvider: errData.email_address_in_use.existing_provider
              , newProvider: provider
              , userEmail: email
              }
      | KSF.Error.serviceUnavailableError err -> do
           Console.error "Service unavailable with SoMe login"
           pure $ Left ServiceUnavailable
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
               user <- finalizeLogin maybeInvalidateCache =<< saveToken loginResponse
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
    Just t  -> Persona.logout t
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

finalizeLogin :: Maybe InvalidateCache -> UserAuth -> Aff (Either UserError Persona.User)
finalizeLogin maybeInvalidateCache auth = do
  userResponse <- try do
    Persona.getUser maybeInvalidateCache auth.userId auth
  case userResponse of
    Left err
      | Just (errData :: Persona.TokenInvalid) <- Api.Error.errorData err -> do
          Console.error "Failed to fetch the user: Invalid token"
          liftEffect deleteToken
          pure $ Left LoginTokenInvalid
      | otherwise -> do
          Console.error "Failed to fetch the user"
          pure $ Left $ UnexpectedError err
    Right user -> do
      Console.info "User fetched successfully"
      pure $ Right user

loadToken :: forall m. MonadEffect m => m (Maybe UserAuth)
loadToken = liftEffect $ runMaybeT do
  authToken <- map Api.Token $ MaybeT $ LocalStorage.getItem "token"
  userId <- map Api.UUID $ MaybeT $ LocalStorage.getItem "uuid"
  pure { userId, authToken }

saveToken :: forall m. MonadEffect m => Persona.LoginResponse -> m UserAuth
saveToken { token, ssoCode, uuid, isAdmin } = liftEffect do
  for_ (Nullable.toMaybe ssoCode) $ \code -> do
    config <- JanrainSSO.loadConfig
    for_ (Nullable.toMaybe config) \conf -> JanrainSSO.setSession conf code
  LocalStorage.setItem "token" case token of Api.Token a -> a
  LocalStorage.setItem "uuid" case uuid of Api.UUID a -> a
  -- This isn't returned by loadToken.
  if isAdmin
    then LocalStorage.setItem "isAdmin" "1"
    else LocalStorage.removeItem "isAdmin"
  pure { userId: uuid, authToken: token }

deleteToken :: Effect Unit
deleteToken = traverse_ LocalStorage.removeItem [ "token", "uuid", "isAdmin" ]

requireToken :: forall m. MonadEffect m => m UserAuth
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
  pausedSub <- try $ Persona.pauseSubscription userUuid subsno startDate endDate =<< requireToken
  case pausedSub of
    Right sub -> pure $ Right sub
    Left err
      | Just (errData :: Persona.InvalidPauseDates) <- Api.Error.errorData err ->
          pure $ Left $ Persona.pauseDateErrorToInvalidDateError errData.invalid_pause_dates.message
      | otherwise -> do
          Console.error "Unexpected error when pausing subscription."
          pure $ Left $ Persona.pauseDateErrorToInvalidDateError Persona.PauseInvalidUnexpected

unpauseSubscription
  :: Api.UUID
  -> Int
  -> Aff Subscription.Subscription
unpauseSubscription userUuid subsno = do
  Persona.unpauseSubscription userUuid subsno =<< requireToken

temporaryAddressChange
  :: Api.UUID
  -> Int
  -> DateTime
  -> Maybe DateTime
  -> String
  -> String
  -> String
  -> Maybe String
  -> Aff (Either Persona.InvalidDateInput Subscription.Subscription)
temporaryAddressChange userUuid subsno startDate endDate streetAddress zipCode countryCode temporaryName = do
  addressChangedSub <- try $ Persona.temporaryAddressChange userUuid subsno startDate endDate streetAddress zipCode countryCode temporaryName =<< requireToken
  case addressChangedSub of
    Right sub -> pure $ Right sub
    Left err
      | Just (errData :: Persona.InvalidDates) <- Api.Error.errorData err ->
          pure $ Left errData.invalid_param.message
      | otherwise -> do
          Console.error "Unexpected error when making temporary address change."
          pure $ Left Persona.InvalidUnexpected

deleteTemporaryAddressChange :: Api.UUID -> Int -> DateTime -> DateTime -> Aff (Either Persona.InvalidDateInput Subscription.Subscription)
deleteTemporaryAddressChange userUuid subsno startDate endDate = do
  tempAddressChangeDeletedSub <- try $ Persona.deleteTemporaryAddressChange userUuid subsno startDate endDate =<< requireToken
  case tempAddressChangeDeletedSub of
    Right sub -> pure $ Right sub
    Left err  -> pure $ Left Persona.InvalidUnexpected

createDeliveryReclamation
  :: Api.UUID
  -> Int
  -> DateTime
  -> PersonaReExport.DeliveryReclamationClaim
  -> Aff (Either Persona.InvalidDateInput Persona.DeliveryReclamation)
createDeliveryReclamation uuid subsno date claim = do
  deliveryReclamation <- try $ Persona.createDeliveryReclamation uuid subsno date claim =<< requireToken
  case deliveryReclamation of
    Right recl -> pure $ Right recl
    Left err -> do
      Console.error "Unexpected error when creating delivery reclamation."
      pure $ Left Persona.InvalidUnexpected

searchUsers
  :: String
  -> Aff (Either String (Array Persona.User))
searchUsers query = do
  users <- try $ Persona.searchUsers query =<< requireToken
  case users of
    Right xs -> pure $ Right xs
    Left err
      | Just (errData :: Persona.Forbidden) <- Api.Error.errorData err ->
          pure $ Left $ errData.forbidden.description
      | otherwise -> do
          Console.error "Unexpected error when searching users"
          pure $ Left "unexpected"

getPayments :: Api.UUID -> Aff (Either String (Array Persona.SubscriptionPayments))
getPayments uuid = do
  payments <- try $ Persona.getPayments uuid =<< requireToken
  case payments of
    Right pay -> pure $ Right pay
    Left err -> do
      Console.error "Unexpected error when getting user payment history "
      pure $ Left "unexpected"


createOrder :: Bottega.NewOrder -> Aff (Either BottegaError Bottega.Order)
createOrder newOrder = callBottega \tokens -> Bottega.createOrder tokens newOrder

payOrder :: Bottega.OrderNumber -> Bottega.PaymentMethod -> Aff (Either BottegaError (Maybe Bottega.PaymentTerminalUrl))
payOrder orderNum paymentMethod = callBottega $ \tokens ->
  Bottega.payOrder tokens orderNum paymentMethod

getOrder :: Bottega.OrderNumber -> Aff (Either BottegaError Bottega.Order)
getOrder orderNum = callBottega $ \tokens -> Bottega.getOrder tokens orderNum

getCreditCards :: Aff (Either BottegaError (Array Bottega.CreditCard))
getCreditCards = callBottega Bottega.getCreditCards

getCreditCard :: Bottega.CreditCardId -> Aff (Either BottegaError Bottega.CreditCard)
getCreditCard creditCardId = callBottega $ \tokens -> Bottega.getCreditCard tokens creditCardId

deleteCreditCard :: Bottega.CreditCardId -> Aff (Either BottegaError Unit)
deleteCreditCard creditCardId = callBottega $ \tokens -> Bottega.deleteCreditCard tokens creditCardId

registerCreditCard :: Aff (Either BottegaError Bottega.CreditCardRegister)
registerCreditCard = callBottega Bottega.registerCreditCard

getCreditCardRegister :: Bottega.CreditCardId -> Bottega.CreditCardRegisterNumber ->  Aff (Either BottegaError Bottega.CreditCardRegister)
getCreditCardRegister creditCardId creditCardRegisterNumber = callBottega $ \tokens -> Bottega.getCreditCardRegister tokens creditCardId creditCardRegisterNumber

updateCreditCardSubscriptions :: Bottega.CreditCardId -> Bottega.CreditCardId -> Aff (Either BottegaError Unit)
updateCreditCardSubscriptions oldCreditCardId newCreditCardId = callBottega $ \tokens -> Bottega.updateCreditCardSubscriptions tokens oldCreditCardId newCreditCardId


callBottega :: forall a. (UserAuth -> Aff a) -> Aff (Either BottegaError a)
callBottega f = do
  tokens <- requireToken
  (try $ f tokens) >>= case _ of
    Right a  -> pure $ Right a
    Left err
      | Just (errData :: Bottega.InsufficientAccount) <- Api.Error.errorData err ->
          pure $ Left BottegaInsufficientAccount
      | otherwise ->
          pure $ Left (BottegaUnexpectedError $ Error.message err)

getPackages :: Aff (Array Package)
getPackages = Bottega.getPackages
