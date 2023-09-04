module Vetrina.Purchase.SetPassword where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api (Password(..))
import KSF.InputField as InputField
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, inputFieldErrorMessage, isFormInvalid, validateField, validateForm, validatePassword, validatePasswordComparison)
import React.Basic (JSX)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, useState, useState', (/\))
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)

type Props =
  { onSuccess :: Effect Unit
  , onError   :: User.UserError -> Effect Unit
  , user      :: User.User
  , logger    :: Sentry.Logger
  }

type PasswordForm =
  { newPassword     :: Maybe String
  , confirmPassword :: Maybe String
  }

data PasswordFormField
  = NewPassword
  | ConfirmPassword (Maybe String)


instance validatableFieldPasswordFormField :: ValidatableField PasswordFormField where
  validateField field value _ = case field of
    NewPassword -> validatePassword NewPassword value
    confirmPw@(ConfirmPassword originalPassword) -> validatePasswordComparison NewPassword confirmPw originalPassword value


component :: Component Props
component = do
  React.component "PurchaseSetPassword" $ \props -> React.do
    form /\ setForm <- useState { newPassword: Nothing
                                , confirmPassword: Nothing
                                }
    isLoading /\ setIsLoading <- useState' Nothing
    let onSubmit = capture_ (submitNewPassword props form setForm setIsLoading)
    pure $ case isLoading of
      Nothing -> render form setForm onSubmit
      Just _ -> Spinner.loadingSpinner

render :: PasswordForm -> ((PasswordForm -> PasswordForm) -> Effect Unit) -> EventHandler -> JSX
render form setForm onSubmit =
  DOM.h1
    { className: "vetrina--headline"
    , children:[ DOM.text "Tack för din beställning!" ]
    }
  <>
  DOM.p
    { className: "vetrina--description-text"
    , children: [ DOM.text "Du är nästan klar! Skriv in önskat lösenord för ditt nya konto nedan." ]
    }
  <> setPasswordForm form setForm onSubmit

setPasswordForm :: PasswordForm -> ((PasswordForm -> PasswordForm) -> Effect Unit) -> EventHandler -> JSX
setPasswordForm form setForm onSubmit =
  DOM.form
    { className: "vetrina--form"
    , onSubmit
    , children:
        [ DOM.div
            { className: "vetrina--input-wrapper"
            , id: "setPassword"
            , children:
                [ InputField.inputField
                   { placeholder: "Önskat lösenord"
                   , type_: InputField.Password
                   , label: Nothing
                   , name: "password"
                   , onChange: \val -> setForm _ { newPassword = val }
                   , value: form.newPassword
                   , validationError: inputFieldErrorMessage $ validateField NewPassword form.newPassword []
                   , inputClass: ""
                   }
                , InputField.inputField
                    { placeholder: "Bekräfta lösenord"
                    , type_: InputField.Password
                    , label: Nothing
                    , name: "confirmPassword"
                    , onChange: \val -> setForm _ { confirmPassword = val }
                    , value: form.confirmPassword
                    , validationError: inputFieldErrorMessage $ validateField (ConfirmPassword form.newPassword) form.confirmPassword []
                    , inputClass: ""
                    }
                ]
            }
        , DOM.input
            { type: "submit"
            , className: "vetrina--button vetrina--completed"
            , disabled: isFormInvalid $ formValidations form
            , value: "Fortsätt"
            }
        ]
    }

submitNewPassword :: Props -> PasswordForm -> ((PasswordForm -> PasswordForm) -> Effect Unit) -> (Maybe Spinner.Loading -> Effect Unit) -> Effect Unit
submitNewPassword props form setForm setIsLoading =
  validateForm (formValidations form) $
    \eitherValidForm -> case eitherValidForm of
      Left _ -> setForm _ { newPassword = form.newPassword <|> Just "" }
      Right validForm
        | Just password <- validForm.newPassword
        , Just confirmPassword <- validForm.confirmPassword
        -> Aff.launchAff_ $ Spinner.withSpinner setIsLoading do
          eitherUser <- User.updatePassword props.user.uuid (Password password) (Password confirmPassword)
          liftEffect $ case eitherUser of
            Left err -> props.onError err
            Right _  -> props.onSuccess
        | otherwise ->
          props.logger.log "Purchase.SetPassword: Tried to submit invalid password form" Sentry.Warning

formValidations :: PasswordForm -> ValidatedForm PasswordFormField PasswordForm
formValidations passwordForm =
  { newPassword: _
  , confirmPassword: _
  }
  <$> validateField NewPassword passwordForm.newPassword []
  <*> validateField (ConfirmPassword passwordForm.newPassword) passwordForm.confirmPassword []
