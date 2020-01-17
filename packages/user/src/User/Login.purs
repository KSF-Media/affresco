module KSF.User.Login where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (foldMap)
import Data.Either (Either(..), either)
import Data.Foldable (surround)
import Data.List.NonEmpty (all)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.Validation.Semigroup (unV)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Facebook.Sdk as FB
import KSF.Button.Component as Button
import KSF.InputField.Component as InputField
import KSF.Registration.Component as Registration
import KSF.User (User, UserError(..))
import KSF.User as User
import KSF.User.Login.Facebook.Success as Facebook.Success
import KSF.User.Login.Google as Google
import KSF.ValidatableForm as Form
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler_)
import React.Basic.Events as Events

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

type Self = React.Self Props State

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

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { initialState, render, didMount }

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
  , launchAff_: \aff -> do
      Aff.launchAff_ do
        Aff.bracket
          (maybe (pure unit) liftEffect $ Nullable.toMaybe jsProps.onLoading)
          (\loading -> maybe (pure unit) liftEffect $ Nullable.toMaybe jsProps.onLoadingEnd)
          (\loading -> aff)
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
  , launchAff_ :: Aff Unit -> Effect Unit
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
  props.launchAff_ $ User.magicLogin \user -> do
    case user of
      Left _ -> self.setState _ { errors { login = Just SomethingWentWrong } }
      Right _ -> pure unit
    props.onUserFetch user

render :: Self -> JSX
render self@{ props, state } =
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

onLogin :: Self -> Form.ValidatedForm LoginField LoginForm -> Effect Unit
onLogin self@{ props, state } = unV
  (\errors -> do
      self.setState _
        { formEmail    = state.formEmail    <|> Just ""
        , formPassword = state.formPassword <|> Just ""
        })
  logUserIn
  where
    logUserIn validForm
      | Just validUsername <- validForm.username
      , Just validPassword <- validForm.password
      , not String.null $ validUsername
      , not String.null $ validPassword
      = props.launchAff_ do
        user <- User.loginTraditional
            { username: validUsername
            , password: validPassword
            , mergeToken: toNullable $ map _.token state.merge
            }
        case user of
          Right _ ->
            -- removing merge token from state in case of success
            liftEffect $ self.setState _ { merge = Nothing }
          Left err ->
            liftEffect $ self.setState _ { errors { login = Just err } }
        -- This call needs to be last, as it will unmount the login component.
        -- (We cannot set state of an unmounted component)
        liftEffect $ props.onUserFetch user
      | otherwise = Log.error "Strange, had validated form with invalid input values"

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
       { onRegister: \affUser -> self.props.launchAff_ do
            user <- affUser
            liftEffect $ self.props.onUserFetch $ Right user
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
        , register
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
            { className: "login--login-social-media-text underline"
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
        , className: "pb2"
        , children:
            [ foldMap formatErrorMessage self.state.errors.login
            , InputField.inputField
                { type_: "text"
                , placeholder: "E-postadress"
                , label: "E-postadress"
                , name: "accountEmail"
                , value: Nothing
                , onChange: \email -> self.setState _ { formEmail = email }
                , validationError:
                   Form.inputFieldErrorMessage $
                     Form.validateField UsernameField self.state.formEmail []
                }
            , InputField.inputField
                { type_: "password"
                , placeholder: "Lösenord"
                , label: "Lösenord"
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
                , disabled: unV (all (not <<< Form.isNotInitialized)) (const false) (loginFormValidations self)
                , value: "Logga in"
                , type: "submit"
                }
            ]
        }
    register :: JSX
    register =
      DOM.div
        { className: "center"
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
            , InputField.inputField
                { type_: "text"
                , placeholder: ""
                , label: ""
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
        onSubmit = Events.handler preventDefault $ \event -> onLogin self $ loginFormValidations self

isSocialLoginEnabled :: Set SocialLoginProvider -> SocialLoginProvider -> Boolean
isSocialLoginEnabled disabledProviders provider = not $ Set.member provider disabledProviders

googleLogin :: Self -> JSX
googleLogin self =
  if not $ Set.member Google self.props.disableSocialLogins
    then
      someLoginButton
        { className: "login--some-button-google"
        , description: "Logga in med Google"
        , onClick: Google.loadGapi { onSuccess: onGoogleLogin, onFailure: onGoogleFailure }
        }
    else mempty
  where
    onGoogleLogin :: Google.AuthResponse -> Effect Unit
    onGoogleLogin { "Zi": { access_token: accessToken }
                  , w3: { "U3": Google.Email email }
                  } = self.props.launchAff_ do
      failOnEmailMismatch self email
      -- setting the email in the state to eventually have it in the merge view
      liftEffect $ self.setState _ { formEmail = Just email }

      user <- User.someAuth self.state.merge (User.Email email) (User.Token accessToken) User.GooglePlus
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
    onGoogleFailure err@{ error: "idpiframe_initialization_failed", details }
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

facebookLogin :: Self -> JSX
facebookLogin self =
  if not $ Set.member Facebook self.props.disableSocialLogins
    then someLoginButton
      { className: "login--some-button-fb"
      , description: "Logga in med Facebook"
      , onClick: onFacebookLogin
      }
    else mempty
  where
    onFacebookLogin :: Effect Unit
    onFacebookLogin = self.props.launchAff_ do
      sdk <- User.facebookSdk
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
            liftEffect $ self.setState _ { errors { login = Just LoginFacebookEmailMissing } }
            throwError err
      failOnEmailMismatch self email
      -- setting the email in the state to eventually send it from the merge view form
      liftEffect $ self.setState _ { formEmail = Just email }
      let (FB.AccessToken fbAccessToken) = accessToken
      user <- User.someAuth self.state.merge (User.Email email) (User.Token fbAccessToken) User.Facebook
      finalizeSomeAuth self user
      liftEffect $ self.props.onUserFetch user

finalizeSomeAuth :: Self -> Either UserError User -> Aff Unit
finalizeSomeAuth self = case _ of
  Right _ ->
    -- removing merge token from state in case of success
    liftEffect $ self.setState _ { merge = Nothing }
  Left (MergeEmailInUse newMergeInfo) -> liftEffect do
    self.props.onMerge
    self.setState _ { merge = Just newMergeInfo }
  Left SomethingWentWrong -> liftEffect $ self.setState _ { errors { login = Just SomethingWentWrong } }
  Left _ -> throwError $ error "An unexpected error occurred during SoMe login"

someLoginButton ::
  { className :: String
  , description :: String
  , onClick :: Effect Unit
  }
  -> JSX
someLoginButton { className, description, onClick } =
  DOM.div
  { className: className <> surround " " additionalClasses
  , children:
    [ Button.button
        { description
        , destination: Nothing
        , onClick
        }
    ]
  }
  where
    additionalClasses = [ "pb1" ]

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
    { className: "underline center mb1"
    , children:
        [ DOM.a
            { href: forgotPasswordUrl
            , children: [ DOM.text "Glömt lösenordet?" ]
            }
        ]
    }

formatErrorMessage :: UserError -> JSX
formatErrorMessage err =
  DOM.div
    { className: "login--error-msg pb1"
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
