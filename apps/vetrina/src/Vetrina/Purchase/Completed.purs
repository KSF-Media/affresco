module Vetrina.Purchase.Completed where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Class.Console as Console
import KSF.InputField.Component as InputField
import KSF.Sentry as Sentry
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, inputFieldErrorMessage, isFormInvalid, validateField, validateForm, validatePassword)
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import Web.HTML (window) as HTML
import Web.HTML.Location (setHref) as HTML
import Web.HTML.Window (location) as HTML

type Props =
  { redirectArticleUrl :: Maybe String
  , user :: Maybe User.User
  , logger :: Sentry.Logger
  }

type State = { passwordForm :: PasswordForm }

type Self = React.Self Props State

component :: React.Component Props
component = React.createComponent "PurchaseCompleted"

completed :: Props -> JSX
completed = make component
  { initialState: { passwordForm: { newPassword: Nothing } }
  , render
  }

type PasswordForm = { newPassword :: Maybe String }
data PasswordFormField = NewPassword
instance validatableFieldPasswordFormField :: ValidatableField PasswordFormField where
  validateField _ value _ = validatePassword NewPassword value

didMount :: Self -> Effect Unit
didMount { props } = do
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
             Just user | not user.hasCompletedRegistration -> setPassword self
             _ -> mempty
        ]
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
        | Just newPw <- validForm.newPassword -> do
          -- TODO: call Persona and login
          case self.props.redirectArticleUrl of
            -- Redirect customer back to the article
            Just url -> HTML.setHref url =<< HTML.location =<< HTML.window
            Nothing -> pure unit
          pure unit
        | otherwise ->
          Console.error "New password seemed OK, but is not defined"

formValidations :: Self -> ValidatedForm PasswordFormField PasswordForm
formValidations self@{ state: { passwordForm } } =
  { newPassword: _ } <$> validateField NewPassword passwordForm.newPassword []
