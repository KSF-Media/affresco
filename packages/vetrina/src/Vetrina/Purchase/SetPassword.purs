module Vetrina.Purchase.SetPassword where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api (Password(..))
import KSF.InputField as InputField
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, inputFieldErrorMessage, isFormInvalid, validateField, validateForm, validatePassword, validatePasswordComparison)
import React.Basic.Classic (JSX, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)

type Props =
  { onSuccess :: Effect Unit
  , onError   :: User.UserError -> Effect Unit
  , user      :: Maybe User.User
  , logger    :: Sentry.Logger
  }

type State =
  { passwordForm :: PasswordForm
  , isLoading    :: Maybe Spinner.Loading
  }

type Self = React.Self Props State

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


component :: React.Component Props
component = React.createComponent "PurchaseSetPassword"

setPassword :: Props -> JSX
setPassword = make component
  { initialState: { passwordForm:
                      { newPassword: Nothing
                      , confirmPassword: Nothing
                      }
                  , isLoading: Nothing
                  }
  , render
  }

didMount :: Self -> Effect Unit
didMount { props } = do
  when (isNothing props.user) do
    -- TODO: Call onError?
    props.logger.log "Did not get user to Purchase.SetPassword phase" Sentry.Warning

render :: Self -> JSX
render self =
  if isJust self.state.isLoading
  then Spinner.loadingSpinner
  else
    DOM.h1
      { className: "vetrina--headline"
      , children:[ DOM.text "Tack för din beställning!" ]
      }
    <> case self.props.user of
        Just _ ->
          DOM.p
            { className: "vetrina--description-text"
            , children: [ DOM.text "Du är nästan klar! Skriv in önskat lösenord för ditt nya konto nedan." ]
            }
          <> setPasswordForm self
        _ -> DOM.text "SOMETHING WENT WRONG!"


setPasswordForm :: Self -> JSX
setPasswordForm self@{ state: { passwordForm } } =
  DOM.form
    { className: "vetrina--form"
    , onSubmit: handler preventDefault $ (\_ -> submitNewPassword self $ formValidations self)
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
                   , onChange: \val -> self.setState _ { passwordForm { newPassword = val } }
                   , value: passwordForm.newPassword
                   , validationError: inputFieldErrorMessage $ validateField NewPassword passwordForm.newPassword []
                   }
                , InputField.inputField
                    { placeholder: "Bekräfta lösenord"
                    , type_: InputField.Password
                    , label: Nothing
                    , name: "confirmPassword"
                    , onChange: \val -> self.setState _ { passwordForm { confirmPassword = val } }
                    , value: passwordForm.confirmPassword
                    , validationError: inputFieldErrorMessage $ validateField (ConfirmPassword passwordForm.newPassword) passwordForm.confirmPassword []
                    }
                ]
            }
        , DOM.input
            { type: "submit"
            , className: "vetrina--button vetrina--completed"
            , disabled: isFormInvalid $ formValidations self
            , value: "Fortsätt"
            }
        ]
    }

submitNewPassword :: Self -> ValidatedForm PasswordFormField PasswordForm -> Effect Unit
submitNewPassword self@{ state: { passwordForm } } form =
  validateForm form $
    \eitherValidForm -> case eitherValidForm of
      Left _ -> self.setState _ { passwordForm { newPassword = passwordForm.newPassword <|> Just "" } }
      Right validForm
        | Just user <- self.props.user
        , Just password <- validForm.newPassword
        , Just confirmPassword <- validForm.confirmPassword
        -> Aff.launchAff_ $ Spinner.withSpinner (self.setState <<< Spinner.setSpinner) do
          eitherUser <- User.updatePassword user.uuid (Password password) (Password confirmPassword)
          liftEffect $ case eitherUser of
            Left err -> self.props.onError err
            Right _  -> self.props.onSuccess
        | otherwise ->
          self.props.logger.log "Purchase.SetPassword: Tried to submit invalid password form" Sentry.Warning

formValidations :: Self -> ValidatedForm PasswordFormField PasswordForm
formValidations { state: { passwordForm } } =
  { newPassword: _
  , confirmPassword: _
  }
  <$> validateField NewPassword passwordForm.newPassword []
  <*> validateField (ConfirmPassword passwordForm.newPassword) passwordForm.confirmPassword []
