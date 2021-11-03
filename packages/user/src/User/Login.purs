module KSF.User.Login where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.List.NonEmpty (all)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.Validation.Semigroup (validation)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Facebook.Sdk as FB
import KSF.Button.Component as Button
import KSF.InputField as InputField
import KSF.Registration.Component as Registration
import KSF.Tracking as Tracking
import KSF.User (User, UserError(..))
import KSF.User as User
import KSF.User.Login.Facebook.Success as Facebook.Success
import KSF.User.Login.Google as Google
import KSF.ValidatableForm as Form
import React.Basic (JSX)
import React.Basic.Classic as React.Classic
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler_)
import React.Basic.Events as Events
import React.Basic.Hooks (Component, UseState, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)


foreign import hideLoginLinks :: Boolean

data SocialLoginProvider = Facebook | Google
derive instance eqSocialLoginOption :: Eq SocialLoginProvider
derive instance ordSocialLoginOption :: Ord SocialLoginProvider

type Errors =
  { login :: Maybe UserError
  , social :: Maybe UserError
  }

type Providers =
  { existing :: User.Provider
  , new :: User.Provider
  }

data LoginField = UsernameField | PasswordField

derive instance eqLoginField :: Eq LoginField

instance validatedLoginField :: Form.ValidatableField LoginField where
  validateField field value serverErrors =
    Form.validateWithServerErrors serverErrors field value
      case field of
        UsernameField -> \_field -> Form.validateEmptyField field "E-postadress krävs."
        PasswordField -> \_field -> Form.validateEmptyField field "Lösenord krävs."

type LoginForm =
  { username :: Maybe String
  , password :: Maybe String
  }

data LoginStep = Login | Registration

type JSProps =
  { onMerge             :: Nullable (Effect Unit)
  , onMergeCancelled    :: Nullable (Effect Unit)
  , onRegister          :: Nullable (Effect Unit)
  , onRegisterCancelled :: Nullable (Effect Unit)
-- TODO:
--  , onLoginSuccess     :: Nullable (EffectFn1 Persona.LoginResponse Unit)
--  , onLoginFail        :: Nullable (EffectFn1 Error Unit)
  , onUserFetchFail     :: Nullable (EffectFn1 String Unit)
  , onUserFetchSuccess  :: Nullable (EffectFn1 User Unit)
  , onLoading           :: Nullable (Effect Unit)
  , onLoadingEnd        :: Nullable (Effect Unit)
  , disableSocialLogins :: Nullable (Array String)
  }

type JSSelf = React.Classic.Self Props State

jsComponent :: Effect (React.ReactComponent JSProps)
jsComponent = React.reactComponent "Login" jsLoginComponent

jsRender :: JSSelf -> JSX
jsRender { props, state, setState } = render { props, state, setState }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { onMerge: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onMerge
  , onMergeCancelled: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onMergeCancelled
  , onRegister: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onRegister
  , onRegisterCancelled: fromMaybe (pure unit) $ Nullable.toMaybe jsProps.onRegisterCancelled
  , onUserFetch:
      userErrorToString >>>
      either
        (maybe (const $ pure unit) runEffectFn1 $ Nullable.toMaybe jsProps.onUserFetchFail)
        (maybe (const $ pure unit) runEffectFn1 $ Nullable.toMaybe jsProps.onUserFetchSuccess)
  , onLogin: \aff -> do
      Aff.launchAff_ do
        Aff.bracket
          (maybe (pure unit) liftEffect $ Nullable.toMaybe jsProps.onLoading)
          (\_loading -> maybe (pure unit) liftEffect $ Nullable.toMaybe jsProps.onLoadingEnd)
          (\_loading -> aff)
  , disableSocialLogins: maybe Set.empty (Set.mapMaybe readSocialLoginProvider <<< Set.fromFoldable) $ Nullable.toMaybe jsProps.disableSocialLogins
  }
  where
    -- TODO: We could have a more descriptive error than just a String
    userErrorToString :: Either UserError User -> Either String User
    userErrorToString = case _ of
      Right user -> Right user
      Left e     -> Left $ show e
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
  , onUserFetch :: Either UserError User -> Effect Unit
  , onLogin :: Aff Unit -> Effect Unit
  , disableSocialLogins :: Set SocialLoginProvider
  }

type State =
  { formEmail :: Maybe String
  , formPassword :: Maybe String
  , errors :: { login :: Maybe UserError
              , social :: Maybe UserError
              }
  , merge :: Maybe User.MergeInfo
  , loginViewStep :: LoginStep
  , socialLoginVisibility :: Visibility
  , loggingIn :: Maybe LoggingIn
  }

data LoggingIn = LoggingIn Int

instance eqLoggingIn :: Eq LoggingIn where
  eq (LoggingIn counterA) (LoggingIn counterB) = counterA == counterB

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  , props :: Props
  }

data Visibility = Visible | Hidden
derive instance eqVisibility :: Eq Visibility

initialState :: State
initialState =
  { formEmail: Nothing
  , formPassword: Nothing
  , errors: { login: Nothing, social: Nothing }
  , merge: Nothing
  , loginViewStep: Login
  , socialLoginVisibility: Hidden
  , loggingIn: Nothing
  }

type LoginHook = React.Render Unit (UseAff (Maybe LoggingIn) Unit (UseState State Unit)) JSX

loginComponent :: Props -> LoginHook
loginComponent props = React.do
  state /\ setState <- useState initialState
  useAff state.loggingIn do
    case state.loggingIn of
      Just (LoggingIn counter) -> do
        Aff.delay $ Milliseconds 500.0
        liftEffect do
          let newCounterValue = if counter == 3 then 0 else counter + 1
          setState \oldState -> oldState { loggingIn = Just $ LoggingIn newCounterValue }
      Nothing -> pure unit
  pure $ render { state, setState, props }

jsLoginComponent :: JSProps -> LoginHook
jsLoginComponent props = loginComponent $ fromJSProps props

login :: Component Props
login = component "Login" loginComponent

render :: Self -> JSX
render self@{ state } =
  case state.merge of
    Nothing        -> renderLogin self
    Just mergeInfo -> renderMerge self mergeInfo

-- | If the email doesn't match the state saved from the Merge request
-- | (so the previous login), we fail
failOnEmailMismatch :: Self -> String -> Aff Unit
failOnEmailMismatch self email
  | isNothing self.state.merge =
      pure unit
  | Just (User.Email email) == map _.userEmail self.state.merge = do
      pure unit
  | otherwise = do
      liftEffect $ self.setState _ { errors { login = Just LoginEmailMismatchError } }
      throwError $ error "Emails don't match"

loginWithRetry :: (Aff (Either UserError User)) -> (User -> Aff Unit) -> (UserError -> Aff Unit) -> (Either UserError User -> Aff Unit) -> Aff Unit
loginWithRetry action handleSuccess handleError handleFinish = do
  result <- withRetry 0
  case result of
    Right user -> handleSuccess user
    Left err -> handleError err
  handleFinish result
  where
    withRetry retry = do
      user <- action
      case user of
        Left ServiceUnavailable | retry < 1 -> do
          delay $ Milliseconds 1000.0
          withRetry $ retry+1
        _ -> pure user

onLogin :: Self -> Form.ValidatedForm LoginField LoginForm -> Effect Unit
onLogin self@{ state, props } =
  validation
    (\_errors -> do
        self.setState _
          { formEmail    = state.formEmail    <|> Just ""
          , formPassword = state.formPassword <|> Just ""
          })
    (\validForm -> self.props.onLogin do
        liftEffect $ self.setState \s -> s { loggingIn = Just $ LoggingIn 0 }
        loginWithRetry (logUserIn validForm) handleSuccess handleError handleFinish)
  where
    logUserIn validForm
      | Just validUsername <- validForm.username
      , Just validPassword <- validForm.password
      , not String.null $ validUsername
      , not String.null $ validPassword
      = User.loginTraditional
          { username: validUsername
          , password: validPassword
          , mergeToken: toNullable $ map _.token state.merge
          }
      | otherwise = do
          Log.error "Strange, had validated form with invalid input values"
          pure $ Left SomethingWentWrong
    handleSuccess user = liftEffect do
      -- removing merge token from state in case of success
      self.setState _ { merge = Nothing }
      Tracking.login (Just user.cusno) "email login" "success"
    handleFinish user =
      -- This call needs to be last, as it will unmount the login component.
      -- (We cannot set state of an unmounted component)
      liftEffect do
        self.setState \s -> s { loggingIn = Nothing }
        props.onUserFetch user

    handleError err = liftEffect do
      self.setState _ { errors { login = Just err } }
      Tracking.login Nothing  "email login" "error"

loginFormValidations :: Self -> Form.ValidatedForm LoginField LoginForm
loginFormValidations self =
  { username: _
  , password: _
  }
  <$> Form.validateField UsernameField self.state.formEmail []
  <*> Form.validateField PasswordField self.state.formPassword []


renderLogin :: Self -> JSX
renderLogin self =
  case self.state.loginViewStep of
    Login        -> renderLoginForm self
    Registration ->
      Registration.registration
       { onRegister: \affUser -> self.props.onLogin do
            user <- affUser
            liftEffect $ self.props.onUserFetch $ Right user
       , onCancelRegistration: do
             self.props.onRegisterCancelled
             self.setState _ { loginViewStep = Login }
        }

renderLoginForm :: Self -> JSX
renderLoginForm self =
  DOM.div
    { className: "login-form"
    , children:
        [ foldMap formatErrorMessage self.state.errors.social
        , loginForm
        , if hideLoginLinks
          then mempty
          else
            DOM.div
              { className: "login--loginLinks"
              , children:
                  [ forgotPassword
                  , forgotEmail
                  , buySubscription
                  ]
              }
        , socialLogins
        ]
    }
  where
    socialLogins :: JSX
    socialLogins =
      -- Don't show possibility of logging with social providers if they're all disabled
      if allSocialLoginsDisabled
      then mempty
      else DOM.div { children: [ loginWithSocial ] <> socialLoginButtons }
      where
        loginWithSocial =
          DOM.span
            { className: "login--login-social-media-text"
            , children: [ DOM.text "Logga in med Facebook eller Google" ]
            , onClick: handler_ $ self.setState _ { socialLoginVisibility = if self.state.socialLoginVisibility == Hidden then Visible else Hidden }
            }
        socialLoginButtons = case self.state.socialLoginVisibility of
          Visible -> [ facebookLogin self, googleLogin self ]
          Hidden  -> mempty
        allSocialLoginsDisabled =
          all (\loginProvider -> Set.member loginProvider self.props.disableSocialLogins) [ Facebook, Google ]

    loginForm :: JSX
    loginForm =
      DOM.form
        { onSubmit: Events.handler preventDefault $ \_ -> onLogin self $ loginFormValidations self
        , children:
            [ foldMap formatErrorMessage self.state.errors.login
            , InputField.inputField
                { type_: InputField.Text
                , placeholder: "E-postadress"
                , label: Just "E-postadress"
                , name: "accountEmail"
                , value: Nothing
                , onChange: \email -> self.setState _ { formEmail = email }
                , validationError:
                   Form.inputFieldErrorMessage $
                     Form.validateField UsernameField self.state.formEmail []
                }
            , InputField.inputField
                { type_: InputField.Password
                , placeholder: "Lösenord"
                , label: Just "Lösenord"
                , name: "accountPassword"
                , value: Nothing
                , onChange: \pw -> self.setState _ { formPassword = pw }
                , validationError:
                   Form.inputFieldErrorMessage $
                     Form.validateField PasswordField self.state.formPassword []
                }
            , DOM.input
                { className: "button-green"
                -- Disable login button only when true errors are found.
                -- This is to prevent it from being disabled (grey) when opening the front page
                , disabled:
                    isJust self.state.loggingIn
                    || validation (all (not <<< Form.isNotInitialized)) (const false) (loginFormValidations self)
                , value: loginValue
                , type: "submit"
                }
            ]
        }
      where
        loginValue :: String
        loginValue =
          case self.state.loggingIn of
            Just (LoggingIn counterValue) -> "Loggar in " <> (String.joinWith "" $ Array.replicate counterValue ".")
            Nothing -> "Logga in"


renderMerge :: Self -> User.MergeInfo -> JSX
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

    mergeActions User.Capture =
      [ DOM.p_
          [ DOM.text captureText
          , DOM.b_ [ DOM.text $ fromMaybe "" self.state.formEmail ]
          , DOM.text "."
          ]
      , mergeAccountForm
      ]
    -- This means user tried to login with Google
    mergeActions User.Facebook =
      [ DOM.p_ [ DOM.text googText ]
      , foldMap formatErrorMessage self.state.errors.social
      , facebookLogin self
      ]
    -- This means user tried to login with Facebook
    mergeActions User.GooglePlus =
      [ DOM.p_ [ DOM.text fbText ]
      , foldMap formatErrorMessage self.state.errors.social
      , googleLogin self
      ]

    cancelButton :: JSX
    cancelButton =
      DOM.div
        { className: "login--cancel"
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
        , className: "login--merge-account"
        , children:
            [ foldMap formatErrorMessage self.state.errors.login
            , InputField.inputField
                { type_: InputField.Text
                , placeholder: ""
                , label: Nothing
                , name: ""
                , value: Nothing
                , onChange: \email -> self.setState _ { formEmail = email }
                , validationError:
                   Form.inputFieldErrorMessage $
                     Form.validateField UsernameField self.state.formEmail []
                }
            , DOM.input
                { className: "button-green"
                , value: "Aktivera"
                , type: "submit"
                }
            ]
        }
      where
        onSubmit = Events.handler preventDefault $ \_event -> onLogin self $ loginFormValidations self

isSocialLoginEnabled :: Set SocialLoginProvider -> SocialLoginProvider -> Boolean
isSocialLoginEnabled disabledProviders provider = not $ Set.member provider disabledProviders

googleLogin :: Self -> JSX
googleLogin self =
  if not $ Set.member Google self.props.disableSocialLogins
    then
      someLoginButton
        { className: "login--some-button login--some-button-google"
        , description: "Logga in med Google"
        , onClick: Google.loadGapi { onSuccess: onGoogleLogin, onFailure: onGoogleFailure }
        }
    else mempty
  where
    onGoogleLogin :: Google.AuthResponse -> Effect Unit
    onGoogleLogin { accessToken, email: (Google.Email email) } = self.props.onLogin do
      failOnEmailMismatch self email
      -- setting the email in the state to eventually have it in the merge view
      liftEffect $ self.setState _ { formEmail = Just email }
      loginWithRetry (logUserIn email accessToken) handleSuccess handleError handleFinish

    logUserIn email accessToken = User.someAuth Nothing self.state.merge (User.Email email) (User.Token accessToken) User.GooglePlus
    handleSuccess user = liftEffect $ Tracking.login (Just user.cusno) "google login" "success"
    handleError _err = liftEffect $ Tracking.login Nothing "google login" "error"
    handleFinish user = do
      finalizeSomeAuth self user
      liftEffect $ self.props.onUserFetch user

    -- | Handles Google login errors. The matched cases are:
    -- | 1) Error "idpiframe_initialization_failed".
    -- |    Google auth initialization failure (probably due to not allowing 3rd party cookies).
    -- | 2) Error "popup_closed_by_user".
    -- |    Ignore this, as it's not an error we want to catch.
    -- | 3) Every other auth failure.
    -- |    TODO: At least some of these should be displayed to the user.
    onGoogleFailure :: Google.Error -> Effect Unit
    onGoogleFailure { error: "idpiframe_initialization_failed", details }
      -- In addition to failing because of third party cookies, the initialization may
      -- fail e.g. for the reason, that the domain is not allowed to perform any google actions.
      -- This case is just to let the developers know what's going on.
      | not $ isGoogleDomainAllowed details
      = do
        Log.error $ "You need to allow this origin, dummy: " <> details
        self.setState _ { errors { login = Just LoginGoogleAuthInitErrorOrigin } }
      | otherwise = self.setState _ { errors { login = Just LoginGoogleAuthInitError } }
    onGoogleFailure { error: "popup_closed_by_user" } = pure unit
    onGoogleFailure _ = Log.error "Google login failed."

    isGoogleDomainAllowed = not <<< contains (Pattern "Not a valid origin for the client")

-- | The Facebook login is disabled. Instead of the button, we just
--   show a text explaining it's gone
facebookLogin :: Self -> JSX
facebookLogin self =
  guard (not $ Set.member Facebook self.props.disableSocialLogins)
    DOM.div
      { className: "login--some-disabled-note"
      , children: [ DOM.text "Möjligheten att logga in med Facebook har tagits bort. Du kan logga in med den e-postadress som är kopplad till din Facebook, "
                  , DOM.a
                      { href: "https://www.hbl.fi/losenord/"
                      , children: [ DOM.text "bara återställ lösenordet här." ]
                      }
                  ]
      }

finalizeSomeAuth :: Self -> Either UserError User -> Aff Unit
finalizeSomeAuth self = case _ of
  Right _ ->
    -- removing merge token from state in case of success
    liftEffect $ self.setState _ { merge = Nothing }
  Left (MergeEmailInUse newMergeInfo) -> liftEffect do
    self.props.onMerge
    self.setState _ { merge = Just newMergeInfo }
  Left SomethingWentWrong -> liftEffect $ self.setState _ { errors { login = Just SomethingWentWrong } }
  Left ServiceUnavailable -> liftEffect $ self.setState _ { errors { login = Just ServiceUnavailable } }
  Left _ -> throwError $ error "An unexpected error occurred during SoMe login"

someLoginButton ::
  { className :: String
  , description :: String
  , onClick :: Effect Unit
  }
  -> JSX
someLoginButton { className, description, onClick } =
  DOM.div
  { className: className
  , children:
    [ Button.button
        { description
        , destination: Nothing
        , onClick
        }
    ]
  }

loginButton :: String -> JSX
loginButton text =
  DOM.input
    { className: "submit-button"
    , type: "submit"
    , value: text
    }

buySubscription :: JSX
buySubscription =
  DOM.div
    { children:
        [ DOM.text "Är du inte prenumerant? "
        , DOM.a
            { className: "login--important"
            , href: "https://prenumerera.ksfmedia.fi/"
            , children: [ DOM.text "Köp en prenumeration!" ]
            }
        ]
    }

forgotPassword :: JSX
forgotPassword =
  DOM.div
    { children:
      [ DOM.text "Glömt lösenordet? "
        , DOM.a
            { href: "https://www.hbl.fi/losenord/"
            , children: [ DOM.text "Klicka här!" ]
            }
        ]
    }

forgotEmail :: JSX
forgotEmail =
  DOM.div
    { children:
      [ DOM.text "Glömt din e-post? "
      , DOM.a
          { className: "login--important"
          , href: "https://www.hbl.fi/kundservice/"
          , children: [ DOM.text "Ta kontakt med vår kundtjänst!" ]
          }
      ]
    }

formatErrorMessage :: UserError -> JSX
formatErrorMessage err =
  DOM.div
    { className: "login--error-msg"
    , children: [ errorMsg ]
    }
  where
    errorMsg =
      case err of
        LoginInvalidCredentials ->
          DOM.text "Kombinationen av e-postadress och lösenord finns inte"
        LoginFacebookEmailMissing ->
          DOM.text "Vi behöver din e-postadress. Godkänn oss i Facebook-inställningar för att få din e-postadress."
        LoginEmailMismatchError ->
          DOM.text "Det finns inget konto med e-postadressen du valde. Var god välj en annan e-postaddress."
        LoginGoogleAuthInitError ->
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
        -- For developers only, should not ever happen in production
        LoginGoogleAuthInitErrorOrigin ->
          DOM.text "Något gick fel vid inloggningen. (Origin not allowed)."
        _ ->
          DOM.text "Något gick fel vid inloggningen. Vänligen försök om en stund igen."
