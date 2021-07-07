module Prenumerera.Register where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), hush)
import Data.List.NonEmpty (all)
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (toMaybe)
import Data.Validation.Semigroup (toEither)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api.Package (Package)
import KSF.CountryDropDown (defaultCountryDropDown)
import KSF.InputField as InputField
import KSF.Registration.Component (RegistrationInputField(..))
import KSF.Registration.Component as Registration
import KSF.User (User)
import KSF.User.Login as Login
import KSF.ValidatableForm as VF
import KSF.ValidatableForm (ValidatedForm, isNotInitialized)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useState, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { user :: Maybe User
  , setUser :: Maybe User -> Effect Unit
  , package :: Package
  , next :: Effect Unit
  , cancel :: Effect Unit
  }

type RegisterData =
  { existingUser :: Boolean
  , form :: Registration.State
  }

initialRegisterData :: Maybe User -> RegisterData
initialRegisterData Nothing =
  { existingUser: false
  , form:
      { formData:
          { emailAddress: Nothing
          , firstName: Nothing
          , lastName: Nothing
          , streetAddress: Nothing
          , zipCode: Nothing
          , city: Nothing
          , country: Just "FI"
            -- Always submitted as Nothing in Prenumerera
          , phone: Nothing
          , password: Nothing
          , confirmPassword: Nothing
          }
      , serverErrors: []
      }
  }

initialRegisterData (Just user) =
  { existingUser: true
  , form:
      { formData:
          { emailAddress: Just user.email
          , firstName: toMaybe user.firstName
          , lastName: toMaybe user.lastName
          , streetAddress: _.streetAddress <$> address
          , zipCode: (toMaybe <<< _.zipCode) =<< address
          , city: (toMaybe <<< _.city) =<< address
          , country: (_.countryCode <$> address) <|> Just "FI"
          , phone: Nothing
            -- Not displayed if existing account
          , password: Nothing
          , confirmPassword: Nothing
          }
      , serverErrors: []
      }
  }
  where
    address = toMaybe user.address

component :: Component Props
component = do
  login <- Login.login
  React.component "Register" $ \ { user, setUser, package, next, cancel } -> React.do
    let loginForm =
          login
            { onMerge: pure unit
            , onMergeCancelled: pure unit
            , onRegister: pure unit
            , onRegisterCancelled: pure unit
            , onUserFetch: userFetched
            , onLogin: const $ pure unit
            , disableSocialLogins: mempty
            }
        userFetched (Right user) = setUser $ Just user
        userFetched (Left _) = pure unit
    loginScreen /\ setLoginScreen <- useState' $ isNothing user
--    loginData /\ setLoginData <- useState $ { email: Nothing, password: Nothing }
    registerData /\ setRegisterData <- useState $ initialRegisterData user
    let onSubmit form = do
          if registerData.existingUser
            then pure unit -- TODO
            else Registration.submitForm registerData.form (setFormState setRegisterData)
                 -- TODO
                 (Aff.runAff_ (setUser <<< hush)) form
          next
    pure $ render package $
      if loginScreen
      then renderLogin loginForm $ setLoginScreen false -- renderLoginScreen login
      else renderRegister registerData setRegisterData onSubmit cancel

render :: Package -> JSX -> JSX
render package content =
  DOM.div
    { className: "container"
    , children:
        [ DOM.h4_ [ DOM.text $ "Din beställning: " <> package.name ]
        , DOM.div
            { id: "ksf-identify"
            , children:
                [ DOM.div
                    { className: "ksf-auth-wrapper"
                    , children:
                        [ DOM.div
                            { className: "ksf-auth-content"
                            , children: [ content ]
                            }
                        ]
                    }
                ]
            }
        ]
    }

renderLogin :: JSX -> Effect Unit -> JSX
renderLogin content startRegister =
  DOM.div
    { id: "ksf-identify-login"
    , children:
        [ DOM.div
            { className: "ksf-auth-header"
            , children:
              -- TODO add image
                [ DOM.h2
                    { className: "dynamicloginPrompt"
                    , id: "loginPrompt"
                    , children: [ DOM.text "Registrera dig eller logga in för att beställa" ]
                    }
                , DOM.p
                    { className: "gift-disclaimer"
                    , children:
                        [ DOM.text "Önskar du ge tidningen i gåva? Kontakta kundservice "
                        , DOM.a { href: "mailto:pren@ksfmedia.fi"
                                , children: [ DOM.text "pren@ksfmedia.fi" ]
                                }
                        , DOM.text " eller på numret "
                        , DOM.a { href: "tel:+35891253500"
                                , children: [ DOM.text "09 125 35 00" ]
                                }
                        , DOM.text " (vardagar kl. 8-12 och 13-16)."
                        ]
                    }
                ]
            }
        , content
        , DOM.div
            { className: "no-account"
            , children:
                [ DOM.text "Ny kund? "
                , DOM.a
                    { href: "#"
                    , children: [ DOM.text "Registrera dig" ]
                    , onClick: handler preventDefault $ const startRegister
                    }
                , DOM.text "!"
                ]
            }
        ]
    }

renderRegister :: RegisterData -> ((RegisterData -> RegisterData) -> Effect Unit) -> (ValidatedForm RegistrationInputField Registration.FormData -> Effect Unit) -> Effect Unit -> JSX
renderRegister reg@{ form } setState save cancel =
  DOM.div
    { className: "janrain-api-container ksf-janrain-user-form"
    , children:
        [ DOM.form
            { className: "janrain-api-form user-form"
            , onSubmit: handler preventDefault $ const $ save $ Registration.formValidations form
            , children:
                (if reg.existingUser then [ DOM.h3_ [ DOM.text "Din information" ] ] else []) <>
                [ row [ inputField FirstName, inputField LastName ]
                , row [ inputField StreetAddress, inputField City ]
                , row [ inputField (Zip (form.formData.country)), inputField Country ]
                , row [ inputField EmailAddress ]
                ] <>
                (case reg.existingUser of
                    false ->
                      [ row [ inputField Password, inputField (ConfirmPassword form.formData.password) ]
                      , row [ mempty, submit ]
                      ]
                    true ->
                      [ DOM.text "todo" ]
                )
            }
        ]
    }
  where
    row xs =
      DOM.div
        { className: "row"
        , children: map rowElement xs
        }
    rowElement x =
      DOM.div
        { className: "column col-xs-12 col-sm-6"
        , children: [ x ]
        }            
    inputField :: RegistrationInputField -> JSX
    inputField EmailAddress = case reg.existingUser of
      false -> Registration.inputField EmailAddress form $ setFormState setState
      true -> InputField.inputField
        { type_: InputField.Email
        , label: Just "E-postadress"
        , placeholder: "E-postadress"
        , name: "email"
        , value: form.formData.emailAddress
        , onChange: const $ pure unit
        , validationError: Nothing
        , disabled: true
        }
    inputField field = Registration.inputField field form $ setFormState setState
    submit =
      React.fragment
        [ if not reg.existingUser then disclaimer else mempty
        , DOM.input
            { type: "submit"
            , className: "submit-button"
            , value: "Fortsätt"
            , disabled: isFormInvalid
            }
        , DOM.label
            { className: "txt-cancel"
            , children:
                [ DOM.text "eller "
                , DOM.a { href: "/", children: [ DOM.text "avbryt" ] }
                ]
            }
        ]
    disclaimer =
      DOM.div
        { className: "disclaimer"
        , children:
            [ DOM.text "Genom att klicka på \"fortsätt\", accepterar du våra "
            , DOM.a
                { href: "https://www.hbl.fi/bruksvillkor/#terms"
                , target: "_blank"
                , children: [ DOM.text "användarvillkor" ]
                }
            , DOM.text " och bekräftar att ha läst och förstått vår "
            , DOM.a
                { href: "https://www.hbl.fi/bruksvillkor/#privacy"
                , target: "_blank"
                , children: [ DOM.text "integritetspolicy" ]
                }
            , DOM.text "."
            ]
        }
    isFormInvalid
      | Left errs <- toEither $ Registration.formValidations form
      = not $ all isNotInitialized errs
      | otherwise = false

setFormState :: ((RegisterData -> RegisterData) -> Effect Unit) -> (Registration.State -> Registration.State) -> Effect Unit
setFormState setState f = setState $ \s -> s { form = f s.form }
