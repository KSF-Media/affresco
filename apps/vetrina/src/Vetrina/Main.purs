module Vetrina.Main where

import Prelude

import React.Basic (JSX)
import React.Basic.Compat as React
import React.Basic.DOM as DOM
import Data.Maybe (Maybe(..))

import KSF.InputField.Component as InputField
import KSF.ValidatableForm as Form

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
    , children: [ emailAddressInput self ]
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
