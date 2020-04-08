module Vetrina.Purchase.NewPurchase where

import Prelude

import Control.Alt ((<|>))
import Data.Array (all, intercalate)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Validation.Semigroup (toEither, unV)
import Effect (Effect)
import Effect.Aff (Aff)
import KSF.InputField.Component as InputField
import KSF.Spinner as Spinner
import KSF.User (PaymentMethod(..), User)
import KSF.User as User
import KSF.ValidatableForm (isNotInitialized)
import KSF.ValidatableForm as Form
import React.Basic (JSX, fragment, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import Vetrina.Types (AccountStatus(..), Product)

type Self = React.Self Props State

type State =
  { newAccountForm      :: NewAccountForm
  , existingAccountForm :: ExistingAccountForm
  , isLoading           :: Maybe Spinner.Loading
  , accountStatus       :: AccountStatus
  , serverErrors        :: Array (Form.ValidationError FormInputField)
  , productSelection    :: Maybe Product
  , paymentMethod       :: Maybe PaymentMethod
  }

type Props =
  { accountStatus :: AccountStatus
  , products :: Array Product
  , mkPurchaseWithNewAccount :: NewAccountForm -> Effect Unit
  , mkPurchaseWithExistingAccount :: ExistingAccountForm -> Effect Unit
  , paymentMethod :: PaymentMethod
  , productSelection :: Product
  -- , description :: String
  -- , productDetails :: String
  -- , form :: JSX
  -- ,
  }

data FormInputField
  = EmailAddress
  | Password
  | ProductSelection

derive instance eqNewAccountInputField :: Eq FormInputField
instance validatableFieldNewAccountInputField :: Form.ValidatableField FormInputField where
  validateField field value serverErrors = case field of
    EmailAddress     -> Form.validateWithServerErrors serverErrors EmailAddress value Form.validateEmailAddress
    Password         -> Form.validatePassword field value
    ProductSelection -> Form.validateEmptyField field "Produkt krävs." value

type PurchaseParameters =
  ( productSelection :: Maybe Product
  , paymentMethod    :: Maybe User.PaymentMethod
  )

type NewAccountForm =
  { emailAddress :: Maybe String
  | PurchaseParameters
  }

type ExistingAccountForm =
  { emailAddress :: Maybe String
  , password     :: Maybe String
  | PurchaseParameters
  }

component :: React.Component Props
component = React.createComponent "NewPurchase"

newPurchase :: Props -> JSX
newPurchase = make component
  { initialState: { newAccountForm:
                      { emailAddress: Nothing
                      , productSelection: Nothing
                      , paymentMethod: Nothing
                      , productSelection: Nothing
                      }
                  , existingAccountForm:
                      { emailAddress: Nothing
                      , password: Nothing
                      , productSelection: Nothing
                      , paymentMethod: Nothing
                      }
                  , isLoading: Just Spinner.Loading -- Let's show spinner until user logged in
                  , accountStatus: NewAccount
                  , serverErrors: []
                  }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self =
  self.setState _ { accountStatus = self.props.accountStatus
                  , paymentMethod = Just self.props.paymentMethod
                  , productSelection = Just self.props.productSelection
                  }

render :: Self -> JSX
render self =
  DOM.div_
    [ DOM.h1_ [ title self.state.accountStatus ]
    , DOM.p
        { className: "vetrina--new-purchase-description"
        , children: [ description self.state.accountStatus ]
        }
    , renderProducts self.props.products
    , form self.state.accountStatus
    ]

title :: AccountStatus -> JSX
title accountStatus = case accountStatus of
  NewAccount           -> DOM.text "Hej kära läsare!"
  ExistingAccount      -> DOM.text "Du har redan ett KSF Media-konto"
  LoggedInAccount user -> DOM.text $ "Hej " <> (fromMaybe "" $ toMaybe user.firstName)

description :: AccountStatus -> JSX
description accountStatus = case accountStatus of
  NewAccount        -> DOM.text "Den här artikeln är exklusiv för våra prenumeranter."
  ExistingAccount   -> DOM.text "Vänligen logga in med ditt KSF Media lösenord."
  LoggedInAccount _ -> DOM.text "Den här artikeln är exklusiv för våra prenumeranter."

renderProducts :: Array Product -> JSX
renderProducts products =
  let descriptions = map ( _.description) products
  in fragment $ map (DOM.p_ <<< Array.singleton <<< intercalate (DOM.br {}) <<< map DOM.text) descriptions

form :: Self -> JSX
form self = case self.state.accountStatus of
  NewAccount        -> newAccountForm self
  ExistingAccount   -> existingAccountForm self
  LoggedInAccount _ -> loggedInAccountForm self
  <> confirmButton self

confirmButton :: Self -> JSX
confirmButton self =
  DOM.input
    { type: "submit"
    , className: "vetrina--button mt2"
    , disabled: isFormInvalid
    , value: "Beställ"
    }
  where
    runValidations = case self.state.accountStatus of
      NewAccount        -> newAccountFormValidations self.state.newAccountForm
      ExistingAccount   -> mempty -- existingAccountFormValidations self.state.existingAccountForm
      LoggedInAccount _ -> loggedInAccountFormValidations { productSelection: self.state.productSelection, paymentMethod: self.state.paymentMethod }
    isFormInvalid
      | Left errs <- toEither $ runValidations
      = not $ all isNotInitialized errs
      | otherwise = false

newAccountForm :: Self -> JSX
newAccountForm self =
  DOM.form
    { className: "vetrina--form"
    , onSubmit: handler preventDefault $
      (\_ -> unV
             (\errors -> self.setState _ { newAccountForm { emailAddress = self.state.newAccountForm.emailAddress <|> Just "" }})
             self.props.mkPurchaseWithNewAccount
             $ newAccountFormValidations self.state.newAccountForm)
    , children:
        [ DOM.p_ [ DOM.text "Börja med att fylla i din e-post." ]
        , emailAddressInput self
        ]
    }

existingAccountForm :: Self -> JSX
existingAccountForm self =
  DOM.form
    { className: "vetrina--form"
    , onSubmit: handler preventDefault $
      (\_ -> unV
             (\errors -> self.setState _ { existingAccountForm
                                             { emailAddress = self.state.existingAccountForm.emailAddress <|> Just ""
                                             , password     = self.state.existingAccountForm.password     <|> Just ""
                                             }})
             self.props.mkPurchaseWithExistingAccount
             $ existingAccountFormValidations self.state.existingAccountForm)
    , children:
        [ emailAddressInput self
        , passwordInput self
        ]
    }

loggedInAccountForm :: Self -> JSX
loggedInAccountForm self =
  DOM.text "d"

emailAddressInput :: Self -> JSX
emailAddressInput self@{ state: { newAccountForm: { emailAddress } }} =
  InputField.inputField
    { type_: InputField.Email
    , label: Nothing
    , name: "emailAddress"
    , placeholder: "E-postadress"
    , onChange: (\val -> self.setState _ { newAccountForm { emailAddress = val }
                                         -- If email value is changed, we must consider it as another
                                         -- attempt of creating a new account (it might be that
                                         -- an account with previous email exists, and we are
                                         -- asking the user to log in right now, so changing
                                         -- the email cancels that)
                                         , existingAccountForm { emailAddress = val, password = Nothing }
                                         , accountStatus = NewAccount
                                         -- Look comment about `existingPassword` above ^
                                         -- Clear server errors of EmailAddress when typing
                                         , serverErrors = Form.removeServerErrors EmailAddress self.state.serverErrors
                                         })
    , validationError: Form.inputFieldErrorMessage $ Form.validateField EmailAddress emailAddress [] --self.state.serverErrors
    , value: emailAddress
    }

passwordInput :: Self -> JSX
passwordInput self = InputField.inputField
  { type_: InputField.Password
  , placeholder: "Lösenord"
  , label: Nothing
  , name: "accountPassword"
  , value: Nothing
  , onChange: \pw -> self.setState _ { existingAccountForm { password = pw } }
  , validationError:
      Form.inputFieldErrorMessage $
      Form.validateField Password self.state.existingAccountForm.password []
  }

acceptTermsCheckbox :: JSX
acceptTermsCheckbox =
  let id    = "accept-terms"
      label = """Jag godkänner KSF Medias användarvillkor och
                 bekräftar att jag har läst och förstått integritets-policyn"""
  in DOM.div
    { className: "vetrina--checbox-container"
    , children:
        [ DOM.input
            { className: "vetrina--checkbox"
            , type: "checkbox"
            , id
            , required: true
            }
        , DOM.label
            { className: "vetrina--checkbox-label"
            , htmlFor: id
            , children: [ DOM.text label ]
            }
        ]
    }


-- confirmButton :: Self -> JSX
-- confirmButton self =
--   DOM.input
--     { type: "submit"
--     , className: "vetrina--button mt2"
--     , disabled: isFormInvalid
--     , value: "Beställ"
--     }
--   where
--     isFormInvalid
--       | Left errs <- toEither $ formValidations self
--       = not $ all isNotInitialized errs
--       | otherwise = false


-- loginToExistingAccount :: Self -> Maybe String -> Maybe String -> Aff (Either OrderFailure User)
-- loginToExistingAccount self (Just username) (Just password) = do
--   let login = { username, password, mergeToken: toNullable Nothing }
--   eitherUser <- User.loginTraditional login
--   case eitherUser of
--     Right u  -> pure $ Right u
--     Left err
--       | User.LoginInvalidCredentials <- err -> pure $ Left AuthenticationError
--       -- TODO: Think about this
--       | User.InvalidFormFields _ <- err -> pure $ Left $ UnrecognizedError "invalid form fields"
--       | User.SomethingWentWrong <- err -> pure $ Left $ ServerError
--       | User.UnexpectedError jsError <- err -> do
--         liftEffect $ self.state.logger.error $ Error.loginError $ message jsError
--         pure $ Left $ ServerError
--       | otherwise -> pure $ Left $ UnrecognizedError ""
-- loginToExistingAccount _ _ _ =
--   pure $ Left $ FormFieldError [ EmailAddress, ExistingPassword ]

newAccountFormValidations :: NewAccountForm -> Form.ValidatedForm FormInputField NewAccountForm
newAccountFormValidations form =
  { emailAddress: _
  , productSelection: form.productSelection
  , paymentMethod: form.paymentMethod
  }
  <$> Form.validateField EmailAddress form.emailAddress []

existingAccountFormValidations :: ExistingAccountForm -> Form.ValidatedForm FormInputField ExistingAccountForm
existingAccountFormValidations form =
  { emailAddress: _
  , password: _
  , productSelection: form.productSelection
  , paymentMethod: form.paymentMethod
  }
  <$> Form.validateField EmailAddress form.emailAddress []
  <*> Form.validateField Password form.password []

loggedInAccountFormValidations :: { | PurchaseParameters } -> Form.ValidatedForm FormInputField { | PurchaseParameters }
loggedInAccountFormValidations form = pure
  { productSelection: form.productSelection
  , paymentMethod: form.paymentMethod
  }
