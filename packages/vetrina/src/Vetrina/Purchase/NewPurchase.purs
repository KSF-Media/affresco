module Vetrina.Purchase.NewPurchase where

import Prelude

import Control.Alt ((<|>))
import Data.Array (all, cons, head)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Nullable (toMaybe)
import Data.Validation.Semigroup (toEither, unV, invalid)
import Effect (Effect)
import KSF.InputField as InputField
import KSF.User (PaymentMethod, User)
import KSF.User as User
import KSF.ValidatableForm (isNotInitialized)
import KSF.ValidatableForm as Form
import React.Basic.Classic (JSX, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import Vetrina.Products as Products
import Vetrina.Types (AccountStatus(..), Product)

type Self = React.Self Props State

type State =
  { newAccountForm ::
       { emailAddress     :: Maybe String
       , acceptLegalTerms :: Boolean
       }
  , existingAccountForm ::
       { emailAddress :: Maybe String
       , password     :: Maybe String
       }
  , accountStatus       :: AccountStatus
  , serverErrors        :: Array (Form.ValidationError FormInputField)
  , errorMessage        :: JSX
  , productSelection    :: Maybe Product
  , paymentMethod       :: Maybe PaymentMethod
  }

type Props =
  { accountStatus                 :: AccountStatus
  , products                      :: Array Product
  , errorMessage                  :: Maybe String
  , mkPurchaseWithNewAccount      :: NewAccountForm -> Effect Unit
  , mkPurchaseWithExistingAccount :: ExistingAccountForm -> Effect Unit
  , mkPurchaseWithLoggedInAccount :: User -> { | PurchaseParameters } -> Effect Unit
  , paymentMethod                 :: PaymentMethod
  , productSelection              :: Maybe Product
  , onLogin                       :: Effect Unit
  }

data FormInputField
  = EmailAddress
  | Password
  | ProductSelection
  | PaymentMethod

derive instance eqNewAccountInputField :: Eq FormInputField
instance validatableFieldNewAccountInputField :: Form.ValidatableField FormInputField where
  validateField field value serverErrors = case field of
    EmailAddress     -> Form.validateWithServerErrors serverErrors EmailAddress value Form.validateEmailAddress
    Password         -> Form.validateEmptyField Password "Lösenord krävs." value
    ProductSelection ->
      -- As `validateField` works currently only with `Maybe Strings`, we need to manually
      -- check the value here (for now). The `value` passed here is maybe the productSelection.id
      if isNothing value
      then invalid (NonEmptyList.singleton (Form.InvalidEmpty ProductSelection "")) -- TODO: Do we need an error message here?
      else pure $ Just mempty
    PaymentMethod    -> Form.noValidation value

type PurchaseParameters =
  ( productSelection :: Maybe Product
  , paymentMethod    :: Maybe User.PaymentMethod
  )

type NewAccountForm =
  { emailAddress     :: Maybe String
  , acceptLegalTerms :: Boolean
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
newPurchase props = make component
  { initialState: { newAccountForm:
                      { emailAddress: Nothing
                      , acceptLegalTerms: false
                      }
                  , existingAccountForm:
                      { emailAddress: Nothing
                      , password: Nothing
                      }
                  , accountStatus: NewAccount
                  , serverErrors: []
                  , errorMessage: foldMap formatErrorMessage props.errorMessage
                  , productSelection: Nothing
                  , paymentMethod: Nothing
                  }
  , render
  , didMount
  }
  props

didMount :: Self -> Effect Unit
didMount self = do
  let maybeExistingUserEmail =
        case self.props.accountStatus of
          ExistingAccount email -> Just email
          _                     -> Nothing
  self.setState _ { accountStatus = self.props.accountStatus
                  , paymentMethod = Just self.props.paymentMethod
                  , productSelection =
                      -- If there's already a selected product, pick that
                      -- or take the first item on the products list
                      self.props.productSelection <|> head self.props.products
                  , existingAccountForm { emailAddress = maybeExistingUserEmail }
                  }

render :: Self -> JSX
render self =
  DOM.h1_ [ title self.state.accountStatus ]
  <> case self.state.accountStatus of
    LoggedInAccount user
      | isNothing $ toMaybe user.firstName ->
        DOM.div
          { className: "vetrina--temporary-user-email"
          , children: [ DOM.text user.email ]
          }
    _ -> mempty
  <> DOM.p
       { className: "vetrina--description-text"
       , children: [ description self.state.accountStatus ]
       }
  <> form self
  <> links self

title :: AccountStatus -> JSX
title accountStatus = case accountStatus of
  NewAccount           -> DOM.text "Hej kära läsare!"
  ExistingAccount _    -> DOM.text "Du har redan ett KSF Media-konto"
  LoggedInAccount user -> DOM.text $ "Hej " <> (fromMaybe "" $ toMaybe user.firstName)

description :: AccountStatus -> JSX
description accountStatus = case accountStatus of
  NewAccount        -> DOM.text "Den här artikeln är exklusiv för våra prenumeranter."
  ExistingAccount _ -> DOM.text "Vänligen logga in med ditt KSF Media lösenord."
  LoggedInAccount _ -> DOM.text "Den här artikeln är exklusiv för våra prenumeranter."

renderProducts :: Self -> JSX
renderProducts self =
  DOM.div
    { className: "vetrina--product-selection-text"
    , children: [ DOM.text "För att läsa artikeln måste du först välja en prenumeration:" ]
    } <>
  Products.product
    { products: self.props.products
    , onProductChange: \p -> self.setState _ { productSelection = Just p }
    , selectedProduct: self.state.productSelection
    }

form :: Self -> JSX
form self = DOM.form $
  { className: "vetrina--form"
  , onSubmit
    -- NOTE: We need to have `emailInput` here (opposed to in `children`),
    -- as we don't want to re-render it when `accountStatus` changes.
    -- This will keep cursor focus in the input field.
  , children:
      [ -- Don't show the product selection if we are asking the user to login
        if not isExistingAccount self.state.accountStatus
           || isNothing self.state.productSelection
        then renderProducts self
        else mempty
      , self.state.errorMessage
      , emailInput self self.state.accountStatus
      ] <> children
  }
  where
    isExistingAccount (ExistingAccount _) = true
    isExistingAccount _ = false

    onSubmit = handler preventDefault $ case self.state.accountStatus of
      NewAccount ->
        (\_ -> unV
          (\errors ->
            self.setState _
              { newAccountForm
                { emailAddress = self.state.newAccountForm.emailAddress <|> Just "" }})
          self.props.mkPurchaseWithNewAccount
          $ newAccountFormValidations self)
      ExistingAccount _ ->
        (\_ -> unV
          (\errors ->
            self.setState _
              { existingAccountForm
                { emailAddress = self.state.existingAccountForm.emailAddress <|> Just ""
                , password     = self.state.existingAccountForm.password     <|> Just ""
                }})
          self.props.mkPurchaseWithExistingAccount
          $ existingAccountFormValidations self)
      LoggedInAccount user ->
        (\_ -> unV
          (\errors -> pure unit)
          (\validForm -> self.props.mkPurchaseWithLoggedInAccount user validForm)
          $ loggedInAccountFormValidations self)
    children = case self.state.accountStatus of
        NewAccount ->
          [ additionalFormRequirements self.state.accountStatus
          , formSubmitButton self
          ]
        ExistingAccount _ ->
          [ passwordInput self
          , formSubmitButton self
          ]
        LoggedInAccount _ ->
          [ formSubmitButton self ]

    additionalFormRequirements NewAccount = acceptTermsCheckbox
    additionalFormRequirements _ = mempty

links :: Self -> JSX
links self =
  DOM.div
    { className: "vetrina--links"
    , children: case self.state.accountStatus of
                  NewAccount        -> loginLink `cons` faqLink <> subscribePagesLink
                  ExistingAccount _ -> resetPasswordLink <> subscribePagesLink
                  LoggedInAccount _ -> faqLink <> subscribePagesLink
    }
  where
    resetPasswordLink :: Array JSX
    resetPasswordLink =
      mkLink "Glömt lösenordet?" "https://www.hbl.fi/losenord/" "Klicka här"

    faqLink :: Array JSX
    faqLink =
      mkLink "Vad är Premium?" "https://www.hbl.fi/fragor-och-svar/" "Frågor och svar"

    loginLink :: JSX
    loginLink =
      DOM.span_
        [ DOM.text "Redan kund? "
        , DOM.span
            { className:"vetrina--login-callback"
            , children: [ DOM.text "Logga in här" ]
            , onClick: handler_ self.props.onLogin
            }
        ]

    subscribePagesLink :: Array JSX
    subscribePagesLink =
      mkLink "" "https://prenumerera.ksfmedia.fi/" "Övriga prenumerationer och betalningssätt"

    mkLink :: String -> String -> String -> Array JSX
    mkLink linkDescription href linkText = Array.singleton $
      DOM.span_
        [ DOM.text $ linkDescription <> " "
        , DOM.a
            { className: "vetrina--link"
            , href
            , children: [ DOM.text linkText ]
            , target: "_blank"
            }
        ]

formSubmitButton :: Self -> JSX
formSubmitButton self =
  DOM.input
    { type: "submit"
    , className: "vetrina--button"
    , disabled
    , value
    }
  where
    value = case self.state.accountStatus of
      NewAccount        -> "Beställ med kreditkort"
      ExistingAccount _ -> "Logga in"
      LoggedInAccount _ -> "Beställ med kreditkort"
    disabled =  case self.state.accountStatus of
      NewAccount        -> isFormInvalid $ newAccountFormValidations self
      ExistingAccount _ -> isFormInvalid $ existingAccountFormValidations self
      LoggedInAccount _ -> isFormInvalid $ loggedInAccountFormValidations self

isFormInvalid :: forall a. Form.ValidatedForm FormInputField a -> Boolean
isFormInvalid validations
  | Left errs <- toEither validations
  = not $ all isNotInitialized errs
  | otherwise = false

formatErrorMessage :: String -> JSX
formatErrorMessage message = InputField.errorMessage message

emailInput :: Self -> AccountStatus -> JSX
emailInput _ (LoggedInAccount _) = mempty
emailInput self accountStatus =
  let emailValue = self.state.existingAccountForm.emailAddress <|> self.state.newAccountForm.emailAddress
  in DOM.div
     { className: "vetrina--input-wrapper vetrina--with-label"
     , children:
         [ case accountStatus of
              NewAccount ->
                DOM.label
                  { className: "vetrina--email-address-label"
                  , htmlFor: "emailAddress"
                  , children: [ DOM.text "Fyll sen i din e-post:" ]
                  }
              _ -> mempty
         , InputField.inputField
             { type_: InputField.Email
             , label: Nothing
             , name: "emailAddress"
             , placeholder: "E-postadress"
             , onChange: onChange
             , validationError: Form.inputFieldErrorMessage $ Form.validateField EmailAddress emailValue self.state.serverErrors
             , value: emailValue
             }
         ]
     }
  where
    onChange = case self.state.accountStatus of
      NewAccount -> \val ->
         self.setState _
           { newAccountForm { emailAddress = val }
           , serverErrors = Form.removeServerErrors EmailAddress self.state.serverErrors
           , errorMessage = mempty
           }
      ExistingAccount _ -> \val ->
        self.setState _
          { existingAccountForm { emailAddress = Nothing, password = Nothing }
          , newAccountForm { emailAddress = val }
            -- If email value is changed, we must consider it as another
            -- attempt of creating a new account (if an account with this email exists,
            -- and we are asking the user to log in right now, changing the email should cancel that)
          , accountStatus = NewAccount
          , serverErrors = Form.removeServerErrors EmailAddress self.state.serverErrors
          , errorMessage = mempty
          }
      _ -> mempty

passwordInput :: Self -> JSX
passwordInput self =
  DOM.div
    { className: "vetrina--input-wrapper"
    , children:
        [ InputField.inputField
            { type_: InputField.Password
            , placeholder: "Lösenord"
            , label: Nothing
            , name: "password"
            , value: self.state.existingAccountForm.password
            , onChange: \pw -> self.setState _ { existingAccountForm { password = pw } }
            , validationError:
              Form.inputFieldErrorMessage $
              Form.validateField Password self.state.existingAccountForm.password []
            }
        ]
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

newAccountFormValidations :: Self -> Form.ValidatedForm FormInputField NewAccountForm
newAccountFormValidations self =
  { emailAddress: _
  , productSelection: _
    -- TODO: Validate this and show error message. We are checking this on server side and with
    -- default browser validation. However, a custom JS validation is missing.
  , acceptLegalTerms: self.state.newAccountForm.acceptLegalTerms
  , paymentMethod: self.state.paymentMethod
  }
  <$> Form.validateField EmailAddress self.state.newAccountForm.emailAddress []
  <*> (Form.validateField ProductSelection (map _.id self.state.productSelection) [] *> pure self.state.productSelection)

existingAccountFormValidations :: Self -> Form.ValidatedForm FormInputField ExistingAccountForm
existingAccountFormValidations self =
  { emailAddress: _
  , password: _
  , productSelection: _
  , paymentMethod: self.state.paymentMethod
  }
  <$> Form.validateField EmailAddress self.state.existingAccountForm.emailAddress []
  <*> Form.validateField Password self.state.existingAccountForm.password []
  <*> (Form.validateField ProductSelection (map _.id self.state.productSelection) [] *> pure self.state.productSelection)

loggedInAccountFormValidations :: Self -> Form.ValidatedForm FormInputField { | PurchaseParameters }
loggedInAccountFormValidations self =
  { productSelection: _
  , paymentMethod: self.state.paymentMethod
  }
  <$> (Form.validateField ProductSelection (map _.id self.state.productSelection) [] *> pure self.state.productSelection)
