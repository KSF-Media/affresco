module KSF.Login.View where

import Prelude

import Data.Array (foldMap)
import Data.Foldable (surround)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import KSF.Button.Component as Button
import KSF.InputField.Component (InputFieldAttributes)
import KSF.InputField.Component as InputField
import KSF.Login.Google (attachClickHandler)
import KSF.Login.Google as Google
import KSF.Login.Login as Login
import Persona as Persona
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler_)
import React.Basic.Events as Events
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended
import Web.DOM.Node as Web.DOM

foreign import loginStyles :: Style

type LoginCallbacks =
  { onLogin :: Effect Unit
  , onFacebookLogin :: Effect Unit
  , onGoogleLogin :: Google.AuthResponse -> Effect Unit
  , onGoogleFailure :: Google.Error -> Effect Unit
  , googleFallbackOnClick :: Effect Unit
  }

type Errors =
  { login :: Maybe Login.Error
  , social :: Maybe Login.Error
  }

type LoginAttributes =
  { login :: LoginCallbacks
  , errors :: Errors
  , onEmailValueChange :: String -> Effect Unit
  , onPasswordValueChange :: String -> Effect Unit
  , loginViewStep :: LoginViewStep
  , showRegistration :: Effect Unit
  , registrationComponent :: JSX
  }

type Providers =
  { existing :: Persona.Provider
  , new :: Persona.Provider
  }

type MergeAttributes =
  { login :: LoginCallbacks
  , errors :: Errors
  , onPasswordValueChange :: String -> Effect Unit
  , onMergeCancelled :: Effect Unit
  , providers :: Providers
  , userEmail :: String
  }

data LoginViewStep = Login | Registration

login :: LoginAttributes -> JSX
login attrs =
    React.Extended.requireStyle
      loginStyles
      $ case attrs.loginViewStep of
          Login -> renderLogin attrs
          Registration -> attrs.registrationComponent

renderLogin :: LoginAttributes -> JSX
renderLogin attrs =
  DOM.div
    { className: "login-form pt2"
    , children:
        [ foldMap formatErrorMessage attrs.errors.social
        , loginForm
        , forgotPassword
        , facebookLogin attrs.login.onFacebookLogin
        , googleLogin
          attrs.login.onGoogleLogin
          attrs.login.onGoogleFailure
          attrs.login.googleFallbackOnClick
        , register
        ]
    }
  where
    loginForm :: JSX
    loginForm =
      DOM.form
        { onSubmit
        , className: "pb2"
        , children:
            [ foldMap formatErrorMessage attrs.errors.login
            , createInputField
                { inputAttributes: emailAttributes
                , className: "email-wrapper"
                , children: []
                , onChange: attrs.onEmailValueChange
                }
            , createInputField
                { inputAttributes: passwordAttributes
                , className: "password-wrapper-and-submit-wrapper"
                , children: [ loginButton "LOGGA IN" ]
                , onChange: attrs.onPasswordValueChange
                }
            ]
        }
      where
        onSubmit = Events.handler preventDefault $ \event -> attrs.login.onLogin

    register :: JSX
    register =
      DOM.div
        { className: "mt3"
        , children:
            [ DOM.text "Inget konto? "
            , DOM.a
                { className: ""
                , href: "#"
                , children: [ DOM.text "Registrera dig!" ]
                , onClick: handler_ attrs.showRegistration
                }
            ]
        }

merge :: MergeAttributes -> JSX
merge attrs =
   DOM.div
     { className: "login-form"
     , children:
         [ DOM.h2
             { className: "mitt-konto--heading"
             , children: [ DOM.text "Har vi setts förr?"]
             }
         , DOM.p_ [ DOM.text topText ]
         ]
         <> mergeActions attrs.providers.existing
         <> [ cancelButton ]
     }
  where
    topText = "Det finns redan ett konto hos oss med samma e-postadress som du har i ditt " <> show attrs.providers.new <> "-konto."
    captureText = "Om du vill, kan du aktivera " <> show attrs.providers.new <> "-inloggning genom att ange lösenordet för "
    fbText = "Om du vill, kan du aktivera Facebook-inloggning genom att logg in med ditt Google-konto."
    googText = "Om du vill, kan du aktivera Google-inloggning genom att logga in med ditt Facebook-konto."

    mergeActions Persona.Capture =
      [ DOM.p_
          [ DOM.text captureText
          , DOM.b_ [ DOM.text attrs.userEmail ]
          , DOM.text "."
          ]
      , mergeAccountForm
      ]
    mergeActions Persona.Facebook =
      [ DOM.p_ [ DOM.text fbText ]
      , foldMap formatErrorMessage attrs.errors.social
      , googleLogin
          attrs.login.onGoogleLogin
          attrs.login.onGoogleFailure
          attrs.login.googleFallbackOnClick
      ]
    mergeActions Persona.GooglePlus =
      [ DOM.p_ [ DOM.text googText ]
      , foldMap formatErrorMessage attrs.errors.social
      , facebookLogin attrs.login.onFacebookLogin
      ]

    cancelButton :: JSX
    cancelButton =
      DOM.div
        { className: "underline center"
        , key: "cancelButton" -- this is here because otherwise React mixes it up with another link
        , children:
          [ DOM.a
              { href: "#"
              , onClick: Events.handler_ attrs.onMergeCancelled
              , children: [ DOM.text "Nej, jag vill inte aktivera" ]
              }
          ]
        }

    mergeAccountForm :: JSX
    mergeAccountForm =
      DOM.form
        { onSubmit
        , className: "pt2 pb2"
        , children:
            [ foldMap formatErrorMessage attrs.errors.login
            , createInputField
                { inputAttributes: passwordAttributes
                , className: "password-wrapper-and-submit-wrapper"
                , children: [ loginButton "AKTIVERA" ]
                , onChange: attrs.onPasswordValueChange
                }
            ]
        }
      where
        onSubmit = Events.handler preventDefault $ \event -> attrs.login.onLogin

googleLogin
  :: (Google.AuthResponse -> Effect Unit)
  -> (Google.Error -> Effect Unit)
  -> Effect Unit
  -> JSX
googleLogin onSuccess onFailure fallbackOnClick =
  someLoginButton
    { className: "login--some-button-google"
    , description: "Logga in med Google"
    , onClick: fallbackOnClick
    , onLoad
    }
  where
    onLoad node = attachClickHandler { node, options: {}, onSuccess, onFailure }

facebookLogin :: Effect Unit -> JSX
facebookLogin onSuccess =
  someLoginButton
    { className: "login--some-button-fb"
    , description: "Logga in med Facebook"
    , onClick: onSuccess
    , onLoad: (\_ -> pure unit)
    }

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
        [ React.element
            InputField.component
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

formatErrorMessage :: Login.Error -> JSX
formatErrorMessage err =
  DOM.div
    { className: "login--error-msg pb1"
    , children: [ errorMsg ]
    }
  where
    errorMsg =
      case err of
        Login.InvalidCredentials ->
          DOM.text "Kombinationen av e-postadress och lösenord finns inte"
        Login.FacebookEmailMissing ->
          DOM.text "Vi behöver din e-postadress. Godkänn oss i Facebook-inställningar för att få din e-postadress."
        Login.EmailMismatchError ->
          DOM.text "Det finns inget konto med e-postadressen du valde. Var god välj en annan e-postaddress."
        Login.GoogleAuthInitError ->
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
        Login.SomethingWentWrong ->
          DOM.text "Något gick fel vid inloggningen. Vänligen försök om en stund igen."
