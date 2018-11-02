module KSF.Login.Component where

import Prelude

import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Class.Console as Log
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import JanrainSSO as JanrainSSO
import LocalStorage as LocalStorage
import KSF.Login.Facebook.Sdk as FB
import KSF.Login.Google as Google
import KSF.Login.Login as Login
import KSF.Login.View as View
import Persona (Token(..))
import Persona as Persona
import React.Basic (JSX)
import React.Basic.Extended as React
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

type JSProps =
  { onMerge            :: Nullable (Effect Unit)
  , onMergeCancelled   :: Nullable (Effect Unit)
-- TODO:
--  , onLoginSuccess     :: Nullable (EffectFn1 Persona.LoginResponse Unit)
--  , onLoginFail        :: Nullable (EffectFn1 Error Unit)
  , onUserFetchFail    :: Nullable (EffectFn1 Error Unit)
  , onUserFetchSuccess :: Nullable (EffectFn1 Persona.User Unit)
  , onLoading          :: Nullable (Effect Unit)
  , onLoadingEnd       :: Nullable (Effect Unit)
  }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { onMerge: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onMerge
  , onMergeCancelled: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onMergeCancelled
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
  }

type Props =
  { onMerge :: Effect Unit
  , onMergeCancelled :: Effect Unit
-- TODO:
--  , onLogin :: Either Error Persona.LoginResponse -> Effect Unit
  , onUserFetch :: Either Error Persona.User -> Effect Unit
  , launchAff_ :: Aff Unit -> Effect Unit
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
  }

initialState :: State
initialState =
  { formEmail: ""
  , formPassword: ""
  , errors: { login: Nothing, social: Nothing, googleAuthInit: Nothing }
  , merge: Nothing
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
component = React.component componentSpec

jsComponent :: React.Component JSProps
jsComponent = React.component $ React.contramapComponentProps fromJSProps componentSpec

componentSpec :: React.ComponentSpec Props State
componentSpec = { displayName: "Login", initialState, receiveProps, render }

receiveProps :: React.ReceivePropsArgs Props State -> Effect Unit
receiveProps { props, state, setState, isFirstMount } = when isFirstMount do
  loadedToken <- loadToken
  props.launchAff_
    case loadedToken of
      Just token -> do
        Console.log "Successfully loaded the saved token from local storage"
        finalizeLogin props token
      Nothing -> liftEffect do
        Console.log "Couldn't load the saved token, giving SSO a try"
        loginConfig <- JanrainSSO.loadConfig
        JanrainSSO.checkSession $ Record.merge
             loginConfig
             { callback_failure: mkEffectFn1 \a -> do
                Console.log "Janrain SSO failure"
             , callback_success: mkEffectFn1 \a -> do
                Console.log "Janrain SSO success"
             , capture_error: mkEffectFn1 \a -> do
                Console.log "Janrain SSO capture error"
             , capture_success: mkEffectFn1 \r@({ result: { accessToken, userData: { uuid } } }) -> do
                Console.log "Janrain SSO capture success"
                Console.log $ unsafeCoerce r
                props.launchAff_ do
                  loginResponse <- Persona.loginSso { accessToken, uuid }
                  finalizeLogin props loginResponse
            }

facebookSdk :: Aff FB.Sdk
facebookSdk = FB.init $ FB.defaultConfig

render :: React.RenderArgs Props State -> JSX
render { props, state, setState } =
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
        }
  where
    onEmailValueChange = \v -> setState \s -> s { formEmail = v }
    onPasswordValueChange = \v -> setState \s -> s { formPassword = v }

    onMergeCancelled = do
      setState \s -> s { merge = Nothing
                       , errors { social = Nothing, login = Nothing } }
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
                   liftEffect $ setState \s -> s
                     { errors { login = Just Login.InvalidCredentials } }
                   throwError err
               | otherwise -> do
                   Console.error "An unexpected error occurred during traditional login"
                   throwError err
      -- removing merge token from state in case of success
      liftEffect $ setState \s -> s { merge = Nothing }
      finalizeLogin props loginResponse

    onFacebookLogin :: Effect Unit
    onFacebookLogin = launchAff_ do
      sdk <- facebookSdk
      FB.StatusInfo { authResponse } <- FB.login loginOptions sdk
      liftEffect $ fetchFacebookUser authResponse sdk
      where
        loginOptions :: FB.LoginOptions
        loginOptions = FB.LoginOptions { scopes: map FB.Scope [ "public_profile", "email" ] }

    onGoogleLogin :: Google.AuthResponse -> Effect Unit
    onGoogleLogin { "Zi": { access_token: accessToken }
                  , w3: { "U3": Google.Email email }
                  } = props.launchAff_ do
      failOnEmailMismatch email
      -- setting the email in the state to eventually have it in the merge view
      liftEffect $ setState \s -> s { formEmail = email }
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
      setState \s -> s { errors { googleAuthInit = Just err } }
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
          setState \s -> s { errors { social = Just Login.GoogleAuthInitError } }
      | otherwise = pure unit

    -- | Fetches user info from Facebook and then Persona.
    -- | Sets response to local storage and invokes `props.onUserFetch`.
    fetchFacebookUser :: Maybe FB.AuthResponse -> FB.Sdk -> Effect Unit
    fetchFacebookUser (Just (FB.AuthResponse { accessToken })) sdk = props.launchAff_ do
      FB.UserInfo { email: FB.UserEmail email } <-
        FB.userInfo accessToken sdk
          `catchError` \err -> do
            -- Here we get an exception if the FB email is missing,
            -- so we have to ask the user
            liftEffect $ setState \s -> s { errors { social = Just Login.FacebookEmailMissing } }
            throwError err
      failOnEmailMismatch email
      -- setting the email in the state to eventually send it from the merge view form
      liftEffect $ setState \s -> s { formEmail = email }
      let (FB.AccessToken fbAccessToken) = accessToken
      userResponse <- someAuth (Persona.Email email) (Token fbAccessToken) Persona.Facebook
      finalizeLogin props userResponse
    fetchFacebookUser Nothing _ = Log.error "Facebook login failed"

    -- | If the email doesn't match the state saved from the Merge request
    -- | (so the previous login), we fail
    failOnEmailMismatch :: String -> Aff Unit
    failOnEmailMismatch email
      | isNothing state.merge =
          pure unit
      | Just (Persona.Email email) == map _.userEmail state.merge = do
          pure unit
      | otherwise = do
          liftEffect $ setState \s -> s { errors { social = Just Login.EmailMismatchError } }
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
                      setState \s -> s
                        { merge = Just
                          { token: errData.email_address_in_use.merge_token
                          , existingProvider: errData.email_address_in_use.existing_provider
                          , newProvider: provider
                          , userEmail: email
                          }
                        }
                    throwError err
                | otherwise -> do
                     Console.error "An unexpected error occurred during SoMe login"
                     throwError err
      -- removing merge token from state in case of success
      liftEffect $ setState \s -> s { merge = Nothing }
      pure responseToken

finalizeLogin :: Props -> Persona.LoginResponse -> Aff Unit
finalizeLogin props loginResponse = do
  saveToken loginResponse
  userResponse <- try do
    Persona.getUser loginResponse.uuid loginResponse.token
  case userResponse of
    -- If fetching fails, token is probably old -> clear local storage
    -- TODO: Actually check the response body
    Left err  -> do
      Console.error "Failed to fetch the user: deleting the token"
      liftEffect deleteToken
      throwError err
    Right user -> do
      Console.info "User fetched successfully"
      pure unit
  liftEffect $ props.onUserFetch userResponse

-- | This is for wrapping whatever action is supposed to happen when the user logs out.
-- | It is then supposed to be passed to the component where the logging out is actually triggered.
logout :: (Maybe Persona.User -> Effect Unit) -> Aff Unit
logout setParentUser = do
  -- Facebook logout is invoked first, as it includes an HTTP request to the Facebook backend.
  -- Before the request has finished, it is not wanted to remove the current user data from local storage or the state of the caller component.
  -- This prevents flickering of the landing page right before the loading indicator is shown.
  logoutFacebook
  logoutGoogle
  liftEffect do
    logoutJanrain
    deleteToken
    setParentUser Nothing

logoutFacebook :: Aff Unit
logoutFacebook = do
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

logoutJanrain :: Effect Unit
logoutJanrain = do
  JanrainSSO.endSession $ Just $ do
    Console.log "Ended janrain sso session"

saveToken :: forall m. MonadEffect m => Persona.LoginResponse -> m Unit
saveToken { token, ssoCode, uuid } = liftEffect do
  for_ (Nullable.toMaybe ssoCode) JanrainSSO.setSession
  LocalStorage.setItem "token" case token of Persona.Token a -> a
  LocalStorage.setItem "uuid" case uuid of Persona.UUID a -> a

loadToken :: Effect (Maybe Persona.LoginResponse)
loadToken = runMaybeT do
  token <- map Persona.Token $ MaybeT $ LocalStorage.getItem "token"
  uuid <- map Persona.UUID $ MaybeT $ LocalStorage.getItem "uuid"
  pure { token, ssoCode: Nullable.toNullable Nothing, uuid }

deleteToken :: Effect Unit
deleteToken = traverse_ LocalStorage.removeItem [ "token", "uuid" ]
