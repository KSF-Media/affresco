module Vetrina.Purchase.Completed where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Exception as Error
import KSF.Api (Password(..))
import KSF.InputField.Component as InputField
import KSF.Sentry as Sentry
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, inputFieldErrorMessage, isFormInvalid, validateField, validateForm, validatePassword)
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)

type Props =
  { onComplete :: Effect Unit
  , onError    :: Error -> Effect Unit
  , user       :: Maybe User.User
  , logger     :: Sentry.Logger
  }

type State = { passwordForm :: PasswordForm, user :: Maybe User.User }

type Self = React.Self Props State

component :: React.Component Props
component = React.createComponent "PurchaseCompleted"

completed :: Props -> JSX
completed = make component
  { initialState: { passwordForm: { newPassword: Nothing }, user: Nothing }
  , render
  }

type PasswordForm = { newPassword :: Maybe String }
data PasswordFormField = NewPassword
instance validatableFieldPasswordFormField :: ValidatableField PasswordFormField where
  validateField _ value _ = validatePassword NewPassword value

didMount :: Self -> Effect Unit
didMount { setState, props } = do
  setState _ { user = props.user }
  when (isNothing props.user) do
    props.logger.log "Did not get user to Purchase.Completed phase" Sentry.Warning

render :: Self -> JSX
render self =
  DOM.div
    { className: "vetrina-purchase-completed--container"
    , children:
        [ DOM.h1_ [ DOM.text "Tack för din prenumeration!" ]
        , DOM.p_ [ DOM.text "Vi har skickat en prenumerationsbekräftelse och instruktioner om hur du tar i bruk våra digitala tjänster till din e-postadress. (Kolla vid behov också i skräppostmappen.)" ]
        , case self.props.user of
             Just user ->
               if not user.hasCompletedRegistration
               then setPassword self
               else completeButton self
             _ -> mempty -- TODO: What do here?
        ]
    }

completeButton :: Self -> JSX
completeButton self =
  DOM.button
    { children: [ DOM.text "OK" ]
    , onClick: handler_ $ self.props.onComplete
    }

setPassword :: Self -> JSX
setPassword self =
  DOM.div
    { className: "vetrina-purchanse-completed--set-password"
    , children: [ setPasswordForm self ]
    }

setPasswordForm :: Self -> JSX
setPasswordForm self@{ state: { passwordForm } } =
  DOM.form
    { className: ""
    , onSubmit: handler preventDefault $ (\_ -> submitNewPassword self $ formValidations self)
    , children:
        [ InputField.inputField
            { placeholder: "Lösenord (minst 6 tecken)"
            , type_: InputField.Password
            , label: "Lösenord"
            , name: "password"
            , onChange: \val -> self.setState _ { passwordForm { newPassword = val } }
            , value: passwordForm.newPassword
            , validationError: inputFieldErrorMessage $ validateField NewPassword passwordForm.newPassword []
            }
        , DOM.input
            { type: "submit"
            , className: "vetrina-purchase-completed--submit-password"
            , disabled: isFormInvalid $ formValidations self
            , value: "Skicka"
            }
        ]
    }

submitNewPassword :: Self -> ValidatedForm PasswordFormField PasswordForm -> Effect Unit
submitNewPassword self@{ state: { passwordForm } } form =
  validateForm form $
    \eitherValidForm -> case eitherValidForm of
      Left errs -> self.setState _ { passwordForm { newPassword = passwordForm.newPassword <|> Just "" } }
      Right validForm
        | Just user <- self.props.user
        , Just password <- validForm.newPassword -> Aff.launchAff_ do
          -- TODO: Add confirm password field
          eitherUser <- User.updatePassword user.uuid (Password password) (Password password)
          liftEffect $ case eitherUser of
            -- TODO: Think about errors
            Left err -> self.props.onError $ Error.error $ "nope"
            Right u -> self.setState _ { user = Just u }
        | otherwise ->
          self.props.logger.log "Purchase.Completed: Tried to submit invalid password form" Sentry.Warning

formValidations :: Self -> ValidatedForm PasswordFormField PasswordForm
formValidations self@{ state: { passwordForm } } =
  { newPassword: _ } <$> validateField NewPassword passwordForm.newPassword []
