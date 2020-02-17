module Vetrina.Main where

import Prelude

import Bottega (Order)
import Bottega as Bottega
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (all)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (foldMap)
import Data.Validation.Semigroup (toEither, unV)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import KSF.InputField.Component as InputField
import KSF.PaymentMethod as PaymentMethod
import KSF.Product (Product)
import KSF.Product as Product
import KSF.User (OrderNumber, PaymentMethod(..), User)
import KSF.User as User
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
  , user :: Maybe User
  }
type Self = React.Self Props State

data NewAccountInputField = EmailAddress
derive instance eqNewAccountInputField :: Eq NewAccountInputField
instance validatableFieldNewAccountInputField :: Form.ValidatableField NewAccountInputField where
  validateField EmailAddress value serverErrors =
    Form.validateWithServerErrors serverErrors EmailAddress value Form.validateEmailAddress

type NewAccountForm =
  { emailAddress     :: Maybe String
  , productSelection :: Product
  , paymentMethod    :: Maybe User.PaymentMethod
  }

app :: React.Component Props
app = React.component
  { displayName: "Vetrina"
  , initialState: { form: { emailAddress: Nothing, productSelection: Product.hblPremium, paymentMethod: Nothing }
                  , serverErrors: []
                  , user: Nothing
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
        , Product.productOption Product.hblPremium true
        , PaymentMethod.paymentMethod (\m -> Console.log $ "AAAAHGH " <> (maybe "" PaymentMethod.paymentMethodString m))
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
  (\validForm -> Aff.launchAff_ $ runExceptT do
      user  <- ExceptT $ createNewAccount self validForm.emailAddress
      order <- ExceptT $ createOrder user self.state.form.productSelection
      a <- ExceptT $ payOrder order $ fromMaybe CreditCard self.state.form.paymentMethod
      pure unit
  )

createNewAccount :: Self -> Maybe String -> Aff (Either String User)
createNewAccount self (Just emailString) = do
  newUser <- User.createUserWithEmail (User.Email emailString)
  case newUser of
    Right user -> do
      liftEffect $ self.setState _ { user = Just user }
      pure $ Right user
    Left User.RegistrationEmailInUse -> do
      -- liftEffect $ self.setState _ { serverErrors = InvalidEmailInUse EmailAddress emailInUseMsg `cons` self.state.serverErrors }
      throwError $ error "email in use"
    Left (User.InvalidFormFields errors) -> do
      -- liftEffect $ handleServerErrs errors
      throwError $ error "invalid form fields"
    _ -> do
      Console.error unexpectedErr
      throwError $ error unexpectedErr
      where
        unexpectedErr = "An unexpected error occurred during registration"
createNewAccount _ Nothing = pure $ Left ""

createOrder :: User -> Product -> Aff (Either String Bottega.Order)
createOrder user product = do
  let newOrder = { packageId: product.id, period: 1, payAmountCents: ceil $ product.price * 100.0 }
  User.createOrder newOrder

payOrder :: Order -> PaymentMethod -> Aff (Either String Order)
payOrder order paymentMethod =
  User.payOrder order.orderNumber paymentMethod

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
  , productSelection: form.productSelection
  , paymentMethod: form.paymentMethod
  }
  <$> Form.validateField EmailAddress form.emailAddress []
