module KSF.User.Login where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (foldMap)
import Data.Either (Either(..), either)
import Data.Foldable (surround)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Facebook.Sdk as FB
import KSF.Button.Component as Button
import KSF.InputField.Component (InputFieldAttributes)
import KSF.InputField.Component as InputField
import KSF.Login.Google (attachClickHandler)
import KSF.Login.Google as Google
import KSF.Registration.Component as Registration
import KSF.User.Login.Facebook.Success as Facebook.Success
import KSF.User.User (User, UserError(..))
import KSF.User.User as User
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler_)
import React.Basic.Events as Events
import Web.DOM.Node as Web.DOM

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
  , onUserFetchFail     :: Nullable (EffectFn1 UserError Unit) -- FIXME: THIS IS BROKEN!
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
  , onUserFetch :: Either UserError User -> Effect Unit
  , launchAff_ :: Aff Unit -> Effect Unit
  , disableSocialLogins :: Set SocialLoginProvider
  }

type State =
  { formEmail :: String
  , formPassword :: String
  , errors :: { login :: Maybe UserError
              , social :: Maybe UserError
              , googleAuthInit :: Maybe Google.Error
              }
  , merge :: Maybe User.MergeInfo
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
  Aff.launchAff_ $ User.automaticLogin \user -> do
    props.onUserFetch user
    case user of
      Left SomethingWentWrong -> self.setState _ { errors { login = Just SomethingWentWrong } }
      _ -> pure unit

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
  | Just (User.Email email) == map _.userEmail self.state.merge = do
      pure unit
  | otherwise = do
      liftEffect $ self.setState _ { errors { login = Just LoginEmailMismatchError } }
      throwError $ error "Emails don't match"

onLogin :: Self -> Effect Unit
onLogin self@{ props, state } = props.launchAff_ do
  user <- User.loginTraditional
            { username: state.formEmail
            , password: state.formPassword
            , mergeToken: toNullable $ map _.token state.merge
            }
  liftEffect $ props.onUserFetch user
  case user of
    Right _ ->
      -- removing merge token from state in case of success
      liftEffect $ self.setState _ { merge = Nothing }
    Left LoginInvalidCredentials ->
      liftEffect $ self.setState _ { errors { login = Just LoginInvalidCredentials } }
    Left SomethingWentWrong ->
      liftEffect $ self.setState _ { errors { login = Just SomethingWentWrong } }
    Left _ ->
      throwError $ error "An unexpected error occurred during traditional login"

renderLogin :: Self -> JSX
renderLogin self =
  case self.state.loginViewStep of
    Login        -> renderLoginForm self
    Registration ->
      Registration.registration
       { onRegister: self.props.launchAff_ <<< void
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
          , DOM.b_ [ DOM.text self.state.formEmail ]
          , DOM.text "."
          ]
      , mergeAccountForm
      ]
    mergeActions User.Facebook =
      [ DOM.p_ [ DOM.text fbText ]
      , foldMap formatErrorMessage self.state.errors.social
      , googleLogin self
      ]
    mergeActions User.GooglePlus =
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
          self.setState _ { errors { login = Just LoginGoogleAuthInitError } }
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
      liftEffect $ self.setState _ { formEmail = email }
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
        _ ->
          DOM.text "Något gick fel vid inloggningen. Vänligen försök om en stund igen."
