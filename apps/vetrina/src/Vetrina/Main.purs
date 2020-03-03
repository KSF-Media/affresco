module Vetrina.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (all, any)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Validation.Semigroup (toEither, unV)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import KSF.Api.Package (Package, PackageName(..))
import KSF.Api.Package as Package
import KSF.InputField.Component as InputField
import KSF.PaymentMethod as PaymentMethod
import KSF.Product (Product)
import KSF.Product.HblPremium as Product
import KSF.Spinner as Spinner
import KSF.User (PaymentMethod(..), User, Order, PaymentTerminalUrl, OrderStatusState(..))
import KSF.User as User
import KSF.ValidatableForm (isNotInitialized)
import KSF.ValidatableForm as Form
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import Vetrina.Purchase.Completed as PurchaseCompleted

type Props = {}

type State =
  { form          :: NewAccountForm
  , serverErrors  :: Array (Form.ValidationError NewAccountInputField)
  , user          :: Maybe User
  , newOrder      :: Maybe Order
  , purchaseState :: PurchaseState
  , poller        :: Aff.Fiber Unit
  , isLoading     :: Maybe Spinner.Loading
  , packages      :: Array Package
  }
type Self = React.Self Props State

type PrevState = { prevProps :: Props, prevState :: State }

data PurchaseState
  = NewPurchase
  | CapturePayment PaymentTerminalUrl
  | ProcessPayment
  | PurchaseFailed
  | PurchaseDone
derive instance eqPurchaseState :: Eq PurchaseState

data NewAccountInputField = EmailAddress
derive instance eqNewAccountInputField :: Eq NewAccountInputField
instance validatableFieldNewAccountInputField :: Form.ValidatableField NewAccountInputField where
  validateField EmailAddress value serverErrors =
    Form.validateWithServerErrors serverErrors EmailAddress value Form.validateEmailAddress

type NewAccountForm =
  { emailAddress     :: Maybe String
  , productSelection :: Maybe Product
  , paymentMethod    :: Maybe User.PaymentMethod
  }

component :: React.Component Props
component = React.createComponent "Vetrina"

app :: Props -> JSX
app = make component
  { initialState: { form: { emailAddress: Nothing, productSelection: Nothing, paymentMethod: Nothing }
                  , serverErrors: []
                  , purchaseState: NewPurchase
                  , user: Nothing
                  , newOrder: Nothing
                  , poller: pure unit
                  , isLoading: Nothing
                  , packages: []
                  }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self = Aff.launchAff_ do
  packages <- User.getPackages
  liftEffect $ self.setState _ { packages = packages }

didUpdate :: Self -> PrevState -> Effect Unit
didUpdate self _ = Aff.launchAff_ $ stopOrderPollerOnCompletedState self

stopOrderPollerOnCompletedState :: Self -> Aff Unit
stopOrderPollerOnCompletedState self =
  when (any (_ == self.state.purchaseState) [ PurchaseFailed, PurchaseDone, NewPurchase ]) $ killOrderPoller self

killOrderPoller :: Self -> Aff Unit
killOrderPoller self = Aff.killFiber (error "Canceled poller") self.state.poller

startOrderPoller :: Self -> Order -> Effect Unit
startOrderPoller self order = do
  newPoller <- Aff.launchAff do
        killOrderPoller self
        newPoller <- Aff.forkAff $ pollOrder self (Right order)
        Aff.joinFiber newPoller
  self.setState _ { poller = newPoller }

pollOrder :: Self -> Either String Order -> Aff Unit
pollOrder self (Right order) = do
  Aff.delay $ Aff.Milliseconds 1000.0
  case order.status.state of
    OrderStarted -> do
      liftEffect $ self.setState _ { purchaseState = ProcessPayment }
      pollOrder self =<< User.getOrder order.number
    OrderCompleted -> liftEffect $ self.setState _ { purchaseState = PurchaseDone }
    OrderFailed    -> liftEffect $ self.setState _ { purchaseState = PurchaseFailed }
    OrderCanceled  -> liftEffect $ self.setState _ { purchaseState = NewPurchase }
    OrderCreated   -> pollOrder self =<< User.getOrder order.number
    UnknownState   -> liftEffect $ self.setState _ { purchaseState = PurchaseFailed }
pollOrder self (Left err) = liftEffect do
  Console.error err
  self.setState _ { purchaseState = PurchaseFailed }

render :: Self -> JSX
render self =
  if isJust self.state.isLoading
  then Spinner.loadingSpinner
  else case self.state.purchaseState of
    NewPurchase ->
      DOM.div
        { className: "vetrina--new-account-container"
        , children: newAccountForm self
            [ emailAddressInput self
            , maybe mempty Product.hblPremium $ Package.findPackage HblPremium self.state.packages
            , PaymentMethod.paymentMethod (\m -> pure unit)
            , confirmButton self
            ]
        }
    (CapturePayment url) -> netsTerminalIframe url
    ProcessPayment -> Spinner.loadingSpinner
    PurchaseFailed -> DOM.text "PURCHASE FAILED :~("
    PurchaseDone -> PurchaseCompleted.completed { redirectArticleUrl: Nothing }

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

setLoading :: Maybe Spinner.Loading -> State -> State
setLoading loading = _ { isLoading = loading }

submitNewAccountForm :: Self -> Form.ValidatedForm NewAccountInputField NewAccountForm -> Effect Unit
submitNewAccountForm self@{ state: { form } } = unV
  (\errors -> self.setState _ { form { emailAddress = form.emailAddress <|> Just "" } })
  (\validForm -> Aff.launchAff_ $ Spinner.withSpinner (self.setState <<< setLoading) do
      eitherRes <- runExceptT do
        user  <- ExceptT $ createNewAccount self validForm.emailAddress
        order <- ExceptT $ createOrder user validForm.productSelection
        paymentUrl <- ExceptT $ payOrder order $ fromMaybe CreditCard self.state.form.paymentMethod
        pure { paymentUrl, order, user }
      case eitherRes of
        Right { paymentUrl, order, user } ->
          liftEffect do
            self.setState _
              { purchaseState = CapturePayment paymentUrl
              , newOrder = Just order
              , user = Just user
              }
            startOrderPoller self order
        Left (err :: String) -> do
          liftEffect do
            Console.error err
            self.setState _ { purchaseState = PurchaseFailed }
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

createOrder :: User -> Maybe Product -> Aff (Either String Order)
createOrder user (Just product) = do
  let newOrder = { packageId: product.id, period: 1, payAmountCents: ceil $ product.price * 100.0 }
  User.createOrder newOrder
createOrder _ Nothing =
  pure $ Left "Tried to create order with no order."

payOrder :: Order -> PaymentMethod -> Aff (Either String PaymentTerminalUrl)
payOrder order paymentMethod =
  User.payOrder order.number paymentMethod

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


netsTerminalIframe :: PaymentTerminalUrl -> JSX
netsTerminalIframe { paymentTerminalUrl } =
  DOM.iframe
    { src: paymentTerminalUrl
    , className: "vetrina--payment-terminal"
    }
