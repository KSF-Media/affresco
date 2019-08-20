module KSF.Login.Component where

import Prelude

import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Parallel (parSequence_)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Class.Console as Log
import Effect.Exception (Error, throw)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Facebook.Sdk as FB
import KSF.JanrainSSO as JanrainSSO
import KSF.LocalStorage as LocalStorage
import KSF.Login.Facebook.Success as Facebook.Success
import KSF.Login.Google as Google
import KSF.Login.Types as Login
import KSF.Login.Types (SocialLoginProvider (..))
import KSF.Login.View as View
import KSF.Registration.Component as Registration
import Persona (Token(..))
import Persona as Persona
import React.Basic (JSX, StateUpdate(..), make, runUpdate)
import React.Basic as React
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import facebookAppId :: String

type Self = React.Self Props State

type JSProps =
  { onMerge             :: Nullable (Effect Unit)
  , onMergeCancelled    :: Nullable (Effect Unit)
  , onRegister          :: Nullable (Effect Unit)
  , onRegisterCancelled :: Nullable (Effect Unit)
-- TODO:
--  , onLoginSuccess     :: Nullable (EffectFn1 Persona.LoginResponse Unit)
--  , onLoginFail        :: Nullable (EffectFn1 Error Unit)
  , onUserFetchFail     :: Nullable (EffectFn1 Error Unit)
  , onUserFetchSuccess  :: Nullable (EffectFn1 Persona.User Unit)
  , onLoading           :: Nullable (Effect Unit)
  , onLoadingEnd        :: Nullable (Effect Unit)
  , disableSocialLogins :: Nullable (Array String)
  }

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { initialState, render, didMount }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { onMerge: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onMerge
  , onMergeCancelled: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onMergeCancelled
  , onRegister: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onRegister
  , onRegisterCancelled: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onRegisterCancelled
  , onUserFetch:
      either
        (maybe (const $ pure unit) runEffectFn1 $ Nullable.toMaybe jsProps.onUserFetchFail)
        (maybe (const $ pure unit) runEffectFn1 $ Nullable.toMaybe jsProps.onUserFetchSuccess)
  , launchAff_: \aff -> do
      Aff.launchAff_ do
        Aff.bracket
          (maybe (pure unit) liftEffect $ Nullable.toMaybe jsProps.onLoading)
          (\loading -> maybe (pure unit) liftEffect $ Nullable.toMaybe jsProps.onLoadingEnd)
          (\loading -> aff)
  , disableSocialLogins: maybe Set.empty (Set.mapMaybe readSocialLoginProvider <<< Set.fromFoldable) $ Nullable.toMaybe jsProps.disableSocialLogins
  }
  where
    readSocialLoginProvider p = case String.toUpper p of
      "GOOGLE"   -> Just Google
      "FACEBOOK" -> Just Facebook
      _          -> Nothing

type Props =
  { onMerge :: Effect Unit
  , onMergeCancelled :: Effect Unit
  , onRegister :: Effect Unit
  , onRegisterCancelled :: Effect Unit
-- TODO:
--  , onLogin :: Either Error Persona.LoginResponse -> Effect Unit
  , onUserFetch :: Either Error Persona.User -> Effect Unit
  , launchAff_ :: Aff Unit -> Effect Unit
  , disableSocialLogins :: Set SocialLoginProvider
  }

type MergeInfo =
  { token :: Persona.MergeToken
  , existingProvider :: Persona.Provider
  , newProvider :: Persona.Provider
  , userEmail :: Persona.Email
  }

type State =
  { formEmail :: String
  , formPassword :: String
  , errors :: { login :: Maybe Login.Error
              , social :: Maybe Login.Error
              , googleAuthInit :: Maybe Google.Error
              }
  , merge :: Maybe MergeInfo
  , loginViewStep :: View.LoginViewStep
  }

data Action =
  LoginError Login.Error
  | GoogleAuthInitError Google.Error
  | FormEmail String
  | FormPassword String
  | MergeCancel
  | SetMergeInfo (Maybe MergeInfo)
  | SetViewStep View.LoginViewStep


initialState :: State
initialState =
  { formEmail: ""
  , formPassword: ""
  , errors: { login: Nothing, social: Nothing, googleAuthInit: Nothing }
  , merge: Nothing
  , loginViewStep: View.Login
  }

type InputFieldAttributes =
  { type_ :: String
  , name :: String
  , placeholder :: String
  , required :: Boolean
  }

forgotPasswordUrl :: String
forgotPasswordUrl = "https://www.hbl.fi/losenord/"

component :: React.Component Props
component = React.createComponent "Login"

login :: Props -> JSX
login = make component
  { initialState
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self@{ props, state } = do
  loadedToken <- loadToken
  props.launchAff_
    case loadedToken of
      Just token -> do
        Console.log "Successfully loaded the saved token from local storage"
        finalizeLogin props token
      Nothing -> liftEffect do
        Console.log "Couldn't load the saved token, giving SSO a try"
        config <- JanrainSSO.loadConfig
        case Nullable.toMaybe config of
          Nothing -> Console.log "sso_lite.js script is not loaded, giving up"
          Just conf -> checkSsoSession conf
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
                props.launchAff_ do
                  loginResponse <-
                    Persona.loginSso { accessToken, uuid } `catchError` case _ of
                      err | Just serverError <- Persona.internalServerError err -> do
                              Console.error "Something went wrong with SSO login"
                              liftEffect $ send self (LoginError Login.SomethingWentWrong)
                              throwError err
                          | otherwise -> do
                              Console.error "An unexpected error occurred during SSO login"
                              throwError err
                  finalizeLogin props loginResponse
            }


update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  LoginError err ->
    Update self.state { errors { login = Just err } }
  GoogleAuthInitError err ->
    Update self.state { errors { googleAuthInit = Just err } }
  FormEmail email ->
    Update self.state { formEmail = email }
  FormPassword password ->
    Update self.state { formPassword = password }
  MergeCancel ->
    Update
      self.state
        { merge = Nothing
        , errors { social = Nothing, login = Nothing }
        }
  SetMergeInfo mergeInfo ->
    Update self.state { merge = mergeInfo }
  SetViewStep step ->
    Update self.state { loginViewStep = step }

send :: Self -> Action -> Effect Unit
send = runUpdate update

facebookSdk :: Aff FB.Sdk
facebookSdk = FB.init $ FB.defaultConfig facebookAppId

render :: Self -> JSX
render self@{ props, state } =
  case state.merge of
    Nothing ->
      View.login
        { login:
            { onLogin
            , onFacebookLogin
            , onGoogleLogin
            , onGoogleFailure
            , googleFallbackOnClick
            }
        , errors:
            { social: state.errors.social
            , login: state.errors.login
            }
        , onEmailValueChange
        , onPasswordValueChange
        , loginViewStep: state.loginViewStep
        , showRegistration: do
            props.onRegister
            send self (SetViewStep View.Registration)
        , registrationComponent:
            Registration.registration
              { onRegister: \registration -> props.launchAff_ do
                   loginResponse <- registration
                   finalizeLogin props loginResponse
              , onCancelRegistration: do
                   props.onRegisterCancelled
                   send self (SetViewStep View.Login)
              }
        , disableSocialLogins: props.disableSocialLogins
        }
    Just mergeInfo ->
      View.merge
        { providers:
            { existing: mergeInfo.existingProvider
            , new: mergeInfo.newProvider
            }
        , userEmail: state.formEmail
        , login:
            { onLogin
            , onFacebookLogin
            , onGoogleLogin
            , onGoogleFailure
            , googleFallbackOnClick
            }
        , onPasswordValueChange
        , errors:
            { social: state.errors.social
            , login: state.errors.login
            }
        , onMergeCancelled
        , disableSocialLogins: props.disableSocialLogins
        }
  where
    onEmailValueChange email =
      send self (FormEmail email)
    onPasswordValueChange password =
      send self (FormPassword password)

    onMergeCancelled = do
      send self MergeCancel
      props.onMergeCancelled

    onLogin :: Effect Unit
    onLogin = props.launchAff_ do
      loginResponse <-
        Persona.login
          { username: state.formEmail
          , password: state.formPassword
          , mergeToken: toNullable $ map _.token state.merge
          } `catchError` case _ of
           err | Just (errData :: Persona.InvalidCredentials) <- Persona.errorData err -> do
                   Console.error errData.invalid_credentials.description
                   liftEffect $ send self (LoginError Login.InvalidCredentials)
                   throwError err
               | Just serverError <- Persona.internalServerError err -> do
                   Console.error "Something went wrong with traditional login"
                   liftEffect $ send self (LoginError Login.SomethingWentWrong)
                   throwError err
               | otherwise -> do
                   Console.error "An unexpected error occurred during traditional login"
                   throwError err
      -- removing merge token from state in case of success
      liftEffect $ send self (SetMergeInfo Nothing)
      finalizeLogin props loginResponse

    onFacebookLogin :: Effect Unit
    onFacebookLogin = props.launchAff_ do
      sdk <- facebookSdk
      FB.StatusInfo { authResponse } <- FB.login loginOptions sdk
      case authResponse of
        Nothing -> do
          liftEffect Facebook.Success.unsetFacebookSuccess
          Log.error "Facebook login failed"
        Just auth -> do
          liftEffect Facebook.Success.setFacebookSuccess
          fetchFacebookUser auth sdk
      where
        loginOptions :: FB.LoginOptions
        loginOptions = FB.LoginOptions { scopes: map FB.Scope [ "public_profile", "email" ] }

    onGoogleLogin :: Google.AuthResponse -> Effect Unit
    onGoogleLogin { "Zi": { access_token: accessToken }
                  , w3: { "U3": Google.Email email }
                  } = props.launchAff_ do
      failOnEmailMismatch email
      -- setting the email in the state to eventually have it in the merge view
      liftEffect $ send self (FormEmail email)
      userResponse <- someAuth (Persona.Email email) (Token accessToken) Persona.GooglePlus
      finalizeLogin props userResponse

    -- | Handles Google login errors. The matched cases are:
    -- | 1) Error "idpiframe_initialization_failed".
    -- |    Google auth initialization failure (probably due to not allowing 3rd party cookies).
    -- | 2) Error "popup_closed_by_user".
    -- |    Ignore this, as it's not an error we want to catch.
    -- | 3) Every other auth failure.
    -- |    TODO: At least some of these should be displayed to the user.
    onGoogleFailure :: Google.Error -> Effect Unit
    onGoogleFailure err@{ error: "idpiframe_initialization_failed" } =
      -- The reason we are not setting `errors.googleAuthInit` here
      -- is that we do not want to show the Google error message to the user
      -- until (if at all) the Google login button is clicked.
      -- This is handled by `googleFallbackOnClick`.
      send self (GoogleAuthInitError err)
    onGoogleFailure { error: "popup_closed_by_user" } = pure unit
    onGoogleFailure _ = Log.error "Google login failed."

    -- | This is to set a fallback onClick behaviour to the Google login button.
    -- | By default, Google wants to attatch its own click handler to the actual
    -- | button element, so the `onClick` attribute is set to do nothing.
    -- | However, if the Google auth initialization fails, we want to add a callback
    -- | to the `onClick` attribute.
    -- | In this case, an error message is shown to the user when the button is clicked.
    googleFallbackOnClick
      | isJust state.errors.googleAuthInit =
          send self (LoginError Login.GoogleAuthInitError)
      | otherwise = pure unit

    -- | Fetches user info from Facebook and then Persona.
    -- | Sets response to local storage and invokes `props.onUserFetch`.
    fetchFacebookUser :: FB.AuthResponse -> FB.Sdk -> Aff Unit
    fetchFacebookUser (FB.AuthResponse { accessToken }) sdk = do
      FB.UserInfo { email: FB.UserEmail email } <-
        FB.userInfo accessToken sdk
          `catchError` \err -> do
            -- Here we get an exception if the FB email is missing,
            -- so we have to ask the user
            liftEffect $ send self (LoginError Login.FacebookEmailMissing)
            throwError err
      failOnEmailMismatch email
      -- setting the email in the state to eventually send it from the merge view form
      liftEffect $ send self (FormEmail email)
      let (FB.AccessToken fbAccessToken) = accessToken
      userResponse <- someAuth (Persona.Email email) (Token fbAccessToken) Persona.Facebook
      finalizeLogin props userResponse

    -- | If the email doesn't match the state saved from the Merge request
    -- | (so the previous login), we fail
    failOnEmailMismatch :: String -> Aff Unit
    failOnEmailMismatch email
      | isNothing state.merge =
          pure unit
      | Just (Persona.Email email) == map _.userEmail state.merge = do
          pure unit
      | otherwise = do
          liftEffect $ send self (LoginError Login.EmailMismatchError)
          throwError $ error "Emails don't match"

    someAuth :: Persona.Email -> Persona.Token -> Persona.Provider -> Aff Persona.LoginResponse
    someAuth email token provider = do
      responseToken <-
        Persona.loginSome
          { provider: show provider
          , someToken: token
          , mergeToken: toNullable $ map _.token state.merge
          } `catchError` case _ of
              err
                -- Merge token handling: in case we get an "email address in use"
                -- error, we set things up in the state to render the merge token screen
                | Just (errData :: Persona.EmailAddressInUse) <- Persona.errorData err -> do
                    Console.error errData.email_address_in_use.description
                    liftEffect do
                      props.onMerge
                      send self (SetMergeInfo
                                  (Just
                                    { token: errData.email_address_in_use.merge_token
                                    , existingProvider: errData.email_address_in_use.existing_provider
                                    , newProvider: provider
                                    , userEmail: email
                                    }))
                    throwError err
                | Just serverError <- Persona.internalServerError err -> do
                    Console.error "Something went wrong with SoMe login"
                    liftEffect $ send self (LoginError Login.SomethingWentWrong)
                    throwError err
                | otherwise -> do
                     Console.error "An unexpected error occurred during SoMe login"
                     throwError err
      -- removing merge token from state in case of success
      liftEffect $ send self (SetMergeInfo Nothing)
      pure responseToken

finalizeLogin :: Props -> Persona.LoginResponse -> Aff Unit
finalizeLogin props loginResponse = do
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
      pure unit
  liftEffect $ props.onUserFetch userResponse

-- | JS-compatible version of 'logout', takes a callback
--   that will be called when it's done.
jsLogout :: Effect Unit -> Effect Unit
jsLogout callback = Aff.runAff_ (\_ -> callback) logout

jsUpdateGdprConsent
  :: Persona.UUID
  -> Persona.Token
  -> Array Persona.GdprConsent
  -> Effect Unit
  -> Effect Unit
jsUpdateGdprConsent uuid token consents callback
  = Aff.runAff_ (\_ -> callback) $ Persona.updateGdprConsent uuid token consents

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
      Console.log "Ended Janrain session"

saveToken :: forall m. MonadEffect m => Persona.LoginResponse -> m Unit
saveToken { token, ssoCode, uuid } = liftEffect do
  for_ (Nullable.toMaybe ssoCode) $ \code -> do
    config <- JanrainSSO.loadConfig
    for_ (Nullable.toMaybe config) \conf -> JanrainSSO.setSession conf code
  LocalStorage.setItem "token" case token of Persona.Token a -> a
  LocalStorage.setItem "uuid" case uuid of Persona.UUID a -> a

loadToken :: forall m. MonadEffect m => m (Maybe Persona.LoginResponse)
loadToken = liftEffect $ runMaybeT do
  token <- map Persona.Token $ MaybeT $ LocalStorage.getItem "token"
  uuid <- map Persona.UUID $ MaybeT $ LocalStorage.getItem "uuid"
  pure { token, ssoCode: Nullable.toNullable Nothing, uuid }

requireToken :: forall m. MonadEffect m => m Persona.LoginResponse
requireToken =
  loadToken >>= case _ of
    Nothing -> liftEffect $ throw "Did not find uuid/token in local storage."
    Just loginResponse -> pure loginResponse

deleteToken :: Effect Unit
deleteToken = traverse_ LocalStorage.removeItem [ "token", "uuid" ]
