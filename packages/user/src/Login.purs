module KSF.User.Login where

import Prelude

import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Parallel (parSequence_)
import Data.Array (foldMap)
import Data.Either (Either(..), either)
import Data.Foldable (for_, surround, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Class.Console as Log
import Effect.Exception (Error, throw)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Facebook.Sdk as FB
import KSF.Button.Component as Button
import KSF.InputField.Component (InputFieldAttributes)
import KSF.InputField.Component as InputField
import KSF.JanrainSSO as JanrainSSO
import KSF.LocalStorage as LocalStorage
import KSF.Login.Google (attachClickHandler)
import KSF.Login.Google as Google
import KSF.Login.Login as Login
import KSF.Registration.Component as Registration
import KSF.User.Facebook.Success as Facebook.Success
import Persona (Token(..))
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Events as Events
import Record as Record
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Node as Web.DOM

data LoginError =
  InvalidCredentials
  | FacebookEmailMissing
  | EmailMismatchError
  | GoogleAuthInitError
  | SomethingWentWrong

data SocialLoginProvider = Facebook | Google
derive instance eqSocialLoginOption :: Eq SocialLoginProvider
derive instance ordSocialLoginOption :: Ord SocialLoginProvider

type Errors =
  { login :: Maybe Login.Error
  , social :: Maybe Login.Error
  }

type Providers =
  { existing :: Persona.Provider
  , new :: Persona.Provider
  }

data LoginStep = Login | Registration

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
  , errors :: { login :: Maybe LoginError
              , social :: Maybe LoginError
              , googleAuthInit :: Maybe Google.Error
              }
  , merge :: Maybe MergeInfo
  , loginViewStep :: LoginStep
  }

initialState :: State
initialState =
  { formEmail: ""
  , formPassword: ""
  , errors: { login: Nothing, social: Nothing, googleAuthInit: Nothing }
  , merge: Nothing
  , loginViewStep: Login
  }

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
                              liftEffect $ self.setState _ { errors { login = Just SomethingWentWrong } }
                              throwError err
                          | otherwise -> do
                              Console.error "An unexpected error occurred during SSO login"
                              throwError err
                  finalizeLogin props loginResponse
            }

facebookSdk :: Aff FB.Sdk
facebookSdk = FB.init $ FB.defaultConfig "asd" --facebookAppId

render :: Self -> JSX
render self@{ props, state } =
  case state.merge of
    Nothing        -> renderLoginForm self
    Just mergeInfo -> renderMerge self mergeInfo

-- | If the email doesn't match the state saved from the Merge request
-- | (so the previous login), we fail
failOnEmailMismatch :: Self -> String -> Aff Unit
failOnEmailMismatch self email
  | isNothing self.state.merge =
      pure unit
  | Just (Persona.Email email) == map _.userEmail self.state.merge = do
      pure unit
  | otherwise = do
      liftEffect $ self.setState _ { errors { login = Just EmailMismatchError } }
      throwError $ error "Emails don't match"

someAuth :: Self -> Persona.Email -> Persona.Token -> Persona.Provider -> Aff Persona.LoginResponse
someAuth self email token provider = do
  responseToken <-
    Persona.loginSome
      { provider: show provider
      , someToken: token
      , mergeToken: toNullable $ map _.token self.state.merge
      } `catchError` case _ of
          err
            -- Merge token handling: in case we get an "email address in use"
            -- error, we set things up in the state to render the merge token screen
            | Just (errData :: Persona.EmailAddressInUse) <- Persona.errorData err -> do
                Console.error errData.email_address_in_use.description
                liftEffect do
                  self.props.onMerge
                  self.setState _
                    { merge =
                         (Just
                           { token: errData.email_address_in_use.merge_token
                           , existingProvider: errData.email_address_in_use.existing_provider
                           , newProvider: provider
                           , userEmail: email
                           })
                    }
                throwError err
            | Just serverError <- Persona.internalServerError err -> do
                Console.error "Something went wrong with SoMe login"
                liftEffect $ self.setState _ { errors { login = Just SomethingWentWrong } }
                throwError err
            | otherwise -> do
                 Console.error "An unexpected error occurred during SoMe login"
                 throwError err
  -- removing merge token from state in case of success
  liftEffect $ self.setState _ { merge = Nothing }
  pure responseToken

onLogin :: Self -> Effect Unit
onLogin self@{ props, state } = props.launchAff_ do
  loginResponse <-
    Persona.login
      { username: state.formEmail
      , password: state.formPassword
      , mergeToken: toNullable $ map _.token state.merge
      } `catchError` case _ of
       err | Just (errData :: Persona.InvalidCredentials) <- Persona.errorData err -> do
               Console.error errData.invalid_credentials.description
               -- liftEffect $ send self (LoginError Login.InvalidCredentials)
               liftEffect $ self.setState _ { errors { login = Just InvalidCredentials } }
               throwError err
           | Just serverError <- Persona.internalServerError err -> do
               Console.error "Something went wrong with traditional login"
               liftEffect $ self.setState _ { errors { login = Just SomethingWentWrong } }
               -- liftEffect $ send self (LoginError Login.SomethingWentWrong)
               throwError err
           | otherwise -> do
               Console.error "An unexpected error occurred during traditional login"
               throwError err
  -- removing merge token from state in case of success
  liftEffect $ self.setState _ { merge = Nothing }
  finalizeLogin props loginResponse

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


renderLogin :: Self -> JSX
renderLogin self =
  case self.state.loginViewStep of
    Login        -> renderLoginForm self
    Registration ->
      Registration.registration
        { onRegister: \registration -> self.props.launchAff_ do
                              loginResponse <- registration
                              finalizeLogin self.props loginResponse
        , onCancelRegistration: do
             self.props.onRegisterCancelled
             self.setState _ { loginViewStep = Login }
        }

renderLoginForm :: Self -> JSX
renderLoginForm self =
  DOM.div
    { className: "login-form pt2"
    , children:
        [ foldMap formatErrorMessage self.state.errors.social
        , loginForm
        , forgotPassword
        , facebookLogin self
        , googleLogin self
        , register
        ]
    }
  where
    loginForm :: JSX
    loginForm =
      DOM.form
        { onSubmit: Events.handler preventDefault $ \event -> onLogin self
        , className: "pb2"
        , children:
            [ foldMap formatErrorMessage self.state.errors.login
            , createInputField
                { inputAttributes: emailAttributes
                , className: "email-wrapper"
                , children: []
                , onChange: \email -> self.setState _ { formEmail = email }
                }
            , createInputField
                { inputAttributes: passwordAttributes
                , className: "password-wrapper-and-submit-wrapper"
                , children: [ loginButton "LOGGA IN" ]
                , onChange: \password -> self.setState _ { formPassword = password }
                }
            ]
        }

    register :: JSX
    register =
      DOM.div
        { className: "mt3 center"
        , children:
            [ DOM.text "Inget konto? "
            , DOM.a
                { className: ""
                , href: "#"
                , children: [ DOM.text "Registrera dig!" ]
                , onClick: handler_ do
                    self.props.onRegister
                    self.setState _ { loginViewStep = Registration }
                }
            ]
        }

renderMerge :: Self -> MergeInfo -> JSX
renderMerge self@{ props } mergeInfo =
   DOM.div
     { className: "login-form"
     , children:
         [ DOM.h2
             { className: "mitt-konto--heading"
             , children: [ DOM.text "Har vi setts förr?"]
             }
         , DOM.p_ [ DOM.text topText ]
         ]
         <> mergeActions mergeInfo.existingProvider
         <> [ cancelButton ]
     }
  where
    topText = "Det finns redan ett konto hos oss med samma e-postadress som du har i ditt " <> show mergeInfo.newProvider <> "-konto."
    captureText = "Om du vill, kan du aktivera " <> show mergeInfo.newProvider <> "-inloggning genom att ange lösenordet för "
    fbText = "Om du vill, kan du aktivera Facebook-inloggning genom att logg in med ditt Google-konto."
    googText = "Om du vill, kan du aktivera Google-inloggning genom att logga in med ditt Facebook-konto."

    mergeActions Persona.Capture =
      [ DOM.p_
          [ DOM.text captureText
          , DOM.b_ [ DOM.text self.state.formEmail ]
          , DOM.text "."
          ]
      , mergeAccountForm
      ]
    mergeActions Persona.Facebook =
      [ DOM.p_ [ DOM.text fbText ]
      , foldMap formatErrorMessage self.state.errors.social
      , googleLogin self
      ]
    mergeActions Persona.GooglePlus =
      [ DOM.p_ [ DOM.text googText ]
      , foldMap formatErrorMessage self.state.errors.social
      , facebookLogin self
      ]

    cancelButton :: JSX
    cancelButton =
      DOM.div
        { className: "underline center"
        , key: "cancelButton" -- this is here because otherwise React mixes it up with another link
        , children:
          [ DOM.a
              { href: "#"
              , onClick: Events.handler_ onMergeCancelled
              , children: [ DOM.text "Nej, jag vill inte aktivera" ]
              }
          ]
        }

    onMergeCancelled = do
      self.setState _ { merge = Nothing, errors { social = Nothing, login = Nothing } }
      props.onMergeCancelled

    mergeAccountForm :: JSX
    mergeAccountForm =
      DOM.form
        { onSubmit
        , className: "pt2 pb2"
        , children:
            [ foldMap formatErrorMessage self.state.errors.login
            , createInputField
                { inputAttributes: passwordAttributes
                , className: "password-wrapper-and-submit-wrapper"
                , children: [ loginButton "AKTIVERA" ]
                , onChange: \email -> self.setState _ { formEmail = email }
                }
            ]
        }
      where
        onSubmit = Events.handler preventDefault $ \event -> onLogin self

isSocialLoginEnabled :: Set SocialLoginProvider -> SocialLoginProvider -> Boolean
isSocialLoginEnabled disabledProviders provider = not $ Set.member provider disabledProviders

googleLogin :: Self -> JSX
googleLogin self =
  if not $ Set.member Google self.props.disableSocialLogins
    then
      someLoginButton
        { className: "login--some-button-google"
        , description: "Logga in med Google"
        , onClick: googleFallbackOnClick
        , onLoad
        }
    else mempty
  where
    onLoad node = attachClickHandler { node, options: {}, onSuccess: onGoogleLogin, onFailure: onGoogleFailure }

    onGoogleLogin :: Google.AuthResponse -> Effect Unit
    onGoogleLogin { "Zi": { access_token: accessToken }
                  , w3: { "U3": Google.Email email }
                  } = self.props.launchAff_ do
      failOnEmailMismatch self email
      -- setting the email in the state to eventually have it in the merge view
      liftEffect $ self.setState _ { formEmail = email }
      userResponse <- someAuth self (Persona.Email email) (Token accessToken) Persona.GooglePlus
      finalizeLogin self.props userResponse

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
      liftEffect $ self.setState _ { errors { googleAuthInit = Just err } }
    onGoogleFailure { error: "popup_closed_by_user" } = pure unit
    onGoogleFailure _ = Log.error "Google login failed."

    -- | This is to set a fallback onClick behaviour to the Google login button.
    -- | By default, Google wants to attatch its own click handler to the actual
    -- | button element, so the `onClick` attribute is set to do nothing.
    -- | However, if the Google auth initialization fails, we want to add a callback
    -- | to the `onClick` attribute.
    -- | In this case, an error message is shown to the user when the button is clicked.
    googleFallbackOnClick
      | isJust self.state.errors.googleAuthInit =
          self.setState _ { errors { login = Just GoogleAuthInitError } }
      | otherwise = pure unit

facebookLogin :: Self -> JSX
facebookLogin self =
  if not $ Set.member Facebook self.props.disableSocialLogins
    then someLoginButton
      { className: "login--some-button-fb"
      , description: "Logga in med Facebook"
      , onClick: onFacebookLogin
      , onLoad: (\_ -> pure unit)
      }
    else mempty
  where
    onFacebookLogin :: Effect Unit
    onFacebookLogin = self.props.launchAff_ do
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

    -- | Fetches user info from Facebook and then Persona.
    -- | Sets response to local storage and invokes `props.onUserFetch`.
    fetchFacebookUser :: FB.AuthResponse -> FB.Sdk -> Aff Unit
    fetchFacebookUser (FB.AuthResponse { accessToken }) sdk = do
      FB.UserInfo { email: FB.UserEmail email } <-
        FB.userInfo accessToken sdk
          `catchError` \err -> do
            -- Here we get an exception if the FB email is missing,
            -- so we have to ask the user
            liftEffect $ self.setState _ { errors { login = Just FacebookEmailMissing } }
            throwError err
      failOnEmailMismatch self email
      -- setting the email in the state to eventually send it from the merge view form
      liftEffect $ self.setState _ { formEmail = email }
      let (FB.AccessToken fbAccessToken) = accessToken
      userResponse <- someAuth self (Persona.Email email) (Token fbAccessToken) Persona.Facebook
      finalizeLogin self.props userResponse

someLoginButton ::
  { className :: String
  , description :: String
  , onClick :: Effect Unit
  , onLoad :: Web.DOM.Node -> Effect Unit
  }
  -> JSX
someLoginButton { className, description, onClick, onLoad } =
  DOM.div
  { className: className <> surround " " additionalClasses
  , children:
    [ Button.button
        { description
        , destination: Nothing
        , onClick
        , onLoad
        }
    ]
  }
  where
    additionalClasses = [ "pb1" ]

createInputField ::
  { inputAttributes :: InputFieldAttributes
  , className :: String
  , children :: Array JSX
  , onChange :: String -> Effect Unit
  }
  -> JSX
createInputField { inputAttributes, className, children, onChange } =
  DOM.div
    { className
    , children:
        [ InputField.inputField
            { type_: inputAttributes.type_
            , placeholder: inputAttributes.placeholder
            , name: inputAttributes.name
            , required: inputAttributes.required
            , children
            , onChange
            , defaultValue: Nothing
            }
        ]
    }

emailAttributes :: InputFieldAttributes
emailAttributes =
  { type_: "email"
  , placeholder: "E-post..."
  , name: "email"
  , required: true
  }

passwordAttributes :: InputFieldAttributes
passwordAttributes =
  { type_: "password"
  , placeholder: "Lösenord..."
  , name: "password"
  , required: true
  }

loginButton :: String -> JSX
loginButton text =
  DOM.input
    { className: "submit-button"
    , type: "submit"
    , value: text
    }

forgotPasswordUrl :: String
forgotPasswordUrl = "https://www.hbl.fi/losenord/"

forgotPassword :: JSX
forgotPassword =
  DOM.div
    { className: "underline center mb3"
    , children:
        [ DOM.a
            { href: forgotPasswordUrl
            , children: [ DOM.text "Glömt lösenordet?" ]
            }
        ]
    }

formatErrorMessage :: LoginError -> JSX
formatErrorMessage err =
  DOM.div
    { className: "login--error-msg pb1"
    , children: [ errorMsg ]
    }
  where
    errorMsg =
      case err of
        InvalidCredentials ->
          DOM.text "Kombinationen av e-postadress och lösenord finns inte"
        FacebookEmailMissing ->
          DOM.text "Vi behöver din e-postadress. Godkänn oss i Facebook-inställningar för att få din e-postadress."
        EmailMismatchError ->
          DOM.text "Det finns inget konto med e-postadressen du valde. Var god välj en annan e-postaddress."
        GoogleAuthInitError ->
          DOM.div_
            [ DOM.text
                """Inloggning med Googlekonto kräver att webbläsaren tillåter tredjepartskakor.
                   Du kan istället logga in med din epostadress. Ange lösenord """
            , DOM.a
                { href: "https://www.hbl.fi/losenord/"
                , target: "_blank"
                , children: [ DOM.text "här" ]
                }
            , DOM.text "."
            ]
        SomethingWentWrong ->
          DOM.text "Något gick fel vid inloggningen. Vänligen försök om en stund igen."
