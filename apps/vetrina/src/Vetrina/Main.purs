module Vetrina.Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (all)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (toEither, unV)
import Effect (Effect)
import Effect.Class.Console as Console
import KSF.InputField.Component as InputField
import KSF.ValidatableForm (isNotInitialized)
import KSF.ValidatableForm as Form
import React.Basic (JSX)
import React.Basic.Compat as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)

type Props = {}
type State =
  { form :: NewAccountForm
  , serverErrors :: Array (Form.ValidationError NewAccountInputField)
  }
type Self = React.Self Props State

data NewAccountInputField = EmailAddress
derive instance eqNewAccountInputField :: Eq NewAccountInputField
instance validatableFieldNewAccountInputField :: Form.ValidatableField NewAccountInputField where
  validateField EmailAddress value serverErrors =
    Form.validateWithServerErrors serverErrors EmailAddress value Form.validateEmailAddress

type NewAccountForm = { emailAddress :: Maybe String }

app :: React.Component Props
app = React.component
  { displayName: "Vetrina"
  , initialState: { form: { emailAddress: Nothing }
                  , serverErrors: []
                  }
  , receiveProps
  , render
  }
  where
    receiveProps _ = do
      pure unit

render :: Self -> JSX
render self =
  DOM.div
    { className: "vetrina--new-account-container"
    , children: newAccountForm self
        [ emailAddressInput self
        , confirmButton self
        ]
    }

newAccountForm :: Self -> Array JSX -> Array JSX
newAccountForm self children =
  Array.singleton $
    DOM.form
      { className: "vetrina--new-account-form"
      , onSubmit: handler preventDefault $ (\_ -> submitNewAccountForm self $ formValidations self)
      , children
      }

emailAddressInput :: Self -> JSX
emailAddressInput self@{ state: { form }} = InputField.inputField
  { type_: "email"
  , label: "E-postadress"
  , name: "emailAddress"
  , placeholder: "E-postadress"
  , onChange: (\val -> self.setState _ { form { emailAddress = val }
                                         -- Clear server errors of EmailAddress when typing
                                       , serverErrors = Form.removeServerErrors EmailAddress self.state.serverErrors
                                       })
  , validationError: Form.inputFieldErrorMessage $ Form.validateField EmailAddress form.emailAddress self.state.serverErrors
  , value: form.emailAddress
  }


submitNewAccountForm :: Self -> Form.ValidatedForm NewAccountInputField NewAccountForm -> Effect Unit
submitNewAccountForm self@{ state: { form } } = unV
  (\errors -> self.setState _ { form { emailAddress = form.emailAddress <|> Just "" } })
  (\validForm -> Console.log "YO!")

confirmButton :: Self -> JSX
confirmButton self =
  DOM.input
    { type: "submit"
    , className: "registration--create-button mt2"
    , disabled: isFormInvalid
    , value: "Skapa konto"
    }
  where
    isFormInvalid
      | Left errs <- toEither $ formValidations self
      = not $ all isNotInitialized errs
      | otherwise = false

formValidations :: Self -> Form.ValidatedForm NewAccountInputField NewAccountForm
formValidations self@{ state: { form } } =
  { emailAddress: _
  }
  <$> Form.validateField EmailAddress form.emailAddress []
