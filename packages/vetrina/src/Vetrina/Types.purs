module Vetrina.Types where

import Prelude

import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), isNothing)
import Data.Validation.Semigroup (invalid)
import KSF.Api.Package (Campaign)
import KSF.User as User
import KSF.ValidatableForm as Form
import React.Basic (JSX)

data AccountStatus
  = NewAccount
  | ExistingAccount String
  | LoggedInAccount User.User

instance eqAccountStatus :: Eq AccountStatus where
  eq NewAccount NewAccount = true
  eq (ExistingAccount _) (ExistingAccount _) = true
  eq (LoggedInAccount _) (LoggedInAccount _) = true
  eq _ _ = false

type ProductContent =
  { title       :: String
  , description :: String
  }

type Product =
  { id                           :: String
  , name                         :: String
  , description                  :: JSX
  , descriptionLoggedInAccount   :: JSX
  , descriptionPurchaseCompleted :: JSX
  , priceCents                   :: Int
  , campaign                     :: Maybe Campaign
  , contents                     :: Array ProductContent
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
    PaymentMethod    -> Form.validateEmptyField PaymentMethod "Betalningssätt krävs." value

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

data OrderFailure
  = EmailInUse String
  | SubscriptionExists
  | RefusedByIssuer
  | InsufficientAccount
  | InitializationError
  | FormFieldError (Array FormInputField)
  | AuthenticationError
  | ServerError
  | UnexpectedError String

derive instance eqOrderFailure :: Eq OrderFailure

data PurchaseState
  = NewPurchase
  | CapturePayment
  | ProcessPayment
  | ScaRequired
  | PurchaseFailed OrderFailure
  | PurchaseSetPassword
  | PurchaseCompleted AccountStatus
  | PurchasePolling

derive instance eqPurchaseState :: Eq PurchaseState
