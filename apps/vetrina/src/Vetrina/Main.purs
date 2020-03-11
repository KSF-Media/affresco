module Vetrina.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (all, any, snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Nullable (toNullable)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (toEither, unV)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import KSF.Api.Package (PackageName(..), PackageValidationError(..))
import KSF.InputField.Component as InputField
import KSF.JSError as Error
import KSF.PaymentMethod as PaymentMethod
import KSF.Product (Product)
import KSF.Product as Product
import KSF.Sentry as Sentry
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

foreign import sentryDsn_ :: Effect String

type Props = {}

type State =
  { form          :: NewAccountForm
  , serverErrors  :: Array (Form.ValidationError NewAccountInputField)
  , user          :: Maybe User
  , newOrder      :: Maybe Order
  , purchaseState :: PurchaseState
  , poller        :: Aff.Fiber Unit
  , isLoading     :: Maybe Spinner.Loading
  , products      :: Array Product
  , accountStatus :: AccountStatus
  , orderFailure  :: Maybe OrderFailure
  , logger        :: Sentry.Logger
  }
type Self = React.Self Props State

type PrevState = { prevProps :: Props, prevState :: State }

data PurchaseState
  = NewPurchase
  | CapturePayment PaymentTerminalUrl
  | ProcessPayment
  | PurchaseFailed
  | PurchaseDone
  | PurchaseUnexpectedError
derive instance eqPurchaseState :: Eq PurchaseState

data NewAccountInputField
  = EmailAddress
  | ExistingPassword
derive instance eqNewAccountInputField :: Eq NewAccountInputField
instance validatableFieldNewAccountInputField :: Form.ValidatableField NewAccountInputField where
  validateField field value serverErrors = case field of
    EmailAddress     -> Form.validateWithServerErrors serverErrors EmailAddress value Form.validateEmailAddress
    ExistingPassword -> Form.validateEmptyField ExistingPassword "Lösenord krävs." value

type NewAccountForm =
  { emailAddress     :: Maybe String
  , existingPassword :: Maybe String
  , productSelection :: Maybe Product
  , paymentMethod    :: User.PaymentMethod
  }

data AccountStatus
  = NewAccount
  | ExistingAccount

data OrderFailure
  = EmailInUse
  | FormFieldError (Array NewAccountInputField)
  | AuthenticationError
  | ServerError
  | UnrecognizedError String

component :: React.Component Props
component = React.createComponent "Vetrina"

app :: Props -> JSX
app = make component
  { initialState: { form:
                      { emailAddress: Nothing
                      , existingPassword: Nothing
                      , productSelection: Nothing
                      , paymentMethod: CreditCard
                      }
                  , serverErrors: []
                  , purchaseState: NewPurchase
                  , user: Nothing
                  , newOrder: Nothing
                  , poller: pure unit
                  , isLoading: Just Spinner.Loading -- Let's show spinner until packages have been fetched
                  , products: []
                  , accountStatus: NewAccount
                  , orderFailure: Nothing
                  , logger: Sentry.emptyLogger
                  }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self = do
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing
  self.setState _ { logger = logger }
  Aff.launchAff_ do
    Aff.finally
      -- When packages have been set, hide loading spinner
      (liftEffect $ self.setState \s -> s { isLoading = Nothing })
      do
        packages <- User.getPackages
        let (Tuple invalidProducts validProducts) =
              map (Product.toProduct packages) productsToShow # partitionValidProducts

        for_ invalidProducts $ \err -> liftEffect $ case err of
          PackageOffersMissing packageName -> do
            logger.error $ Error.packageError $ "Missing offers in package: " <> show packageName
          PackageNotFound packageName -> do
            logger.error $ Error.packageError $ "Did not find package from server: " <> show packageName

        case Array.head validProducts of
          Just p -> liftEffect $ self.setState _
                       { products = validProducts
                       , form { productSelection = Just p }
                       }
          -- Did not get any valid packages from the server
          Nothing -> liftEffect do
            logger.error $ Error.packageError "Could not show any products to customer."
            self.setState _ { purchaseState = PurchaseUnexpectedError }

-- TODO: `partitionEithers` could be in some util module
partitionValidProducts :: Array (Either PackageValidationError Product) -> Tuple (Array PackageValidationError) (Array Product)
partitionValidProducts = Array.foldl
  (\(Tuple lefts rights) eitherProduct ->
    case eitherProduct of
      Right p  -> Tuple lefts              (rights `snoc` p)
      Left err -> Tuple (lefts `snoc` err) rights)
  (Tuple [] [])

productsToShow :: Array PackageName
productsToShow = [ HblPremium ]

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
pollOrder self@{ state: { logger } } (Right order) = do
  Aff.delay $ Aff.Milliseconds 1000.0
  case order.status.state of
    OrderStarted -> do
      liftEffect $ self.setState _ { purchaseState = ProcessPayment }
      pollOrder self =<< User.getOrder order.number
    OrderCompleted -> liftEffect $ self.setState _ { purchaseState = PurchaseDone }
    OrderFailed    -> liftEffect do
      logger.error $ Error.orderError "Order failed for customer"
      self.setState _ { purchaseState = PurchaseFailed }
    OrderCanceled  -> liftEffect do
      self.state.logger.log "Customer canceled order" Sentry.Info
      self.setState _ { purchaseState = NewPurchase }
    OrderCreated   -> pollOrder self =<< User.getOrder order.number
    UnknownState   -> liftEffect do
      logger.error $ Error.orderError "Got UnknownState from server"
      self.setState _ { purchaseState = PurchaseFailed }
pollOrder { setState, state: { logger } } (Left err) = liftEffect do
  logger.error $ Error.orderError $ "Failed to get order from server: " <> err
  setState _ { purchaseState = PurchaseFailed }

render :: Self -> JSX
render self =
  if isJust self.state.isLoading
  then Spinner.loadingSpinner
  else case self.state.purchaseState of
    NewPurchase ->
      DOM.div
        { className: "vetrina--new-account-container"
        , children: newAccountForm self
            [ foldMap orderErrorMessage self.state.orderFailure
            , emailAddressInput self
            , case self.state.accountStatus of
                NewAccount      -> mempty
                ExistingAccount -> passwordInput self
            , maybe mempty Product.productRender self.state.form.productSelection
            , PaymentMethod.paymentMethod (\m -> pure unit)
            , confirmButton self
            ]
        }
    (CapturePayment url) -> netsTerminalIframe url
    ProcessPayment -> Spinner.loadingSpinner
    PurchaseFailed -> DOM.text "PURCHASE FAILED :~("
    PurchaseDone -> PurchaseCompleted.completed { redirectArticleUrl: Nothing }
    PurchaseUnexpectedError -> DOM.text "SOMETHING WENT HORRIBLY WRONG SERVER SIDE"

orderErrorMessage :: OrderFailure -> JSX
orderErrorMessage failure =
  case failure of
    AuthenticationError -> InputField.errorMessage "Kombinationen av e-postadress och lösenord finns inte"
    EmailInUse -> DOM.text "Email already exists, please log in" -- TODO: Waiting for copy
    _ -> DOM.text "Något gick fel vid inloggningen. Vänligen försök om en stund igen."

newAccountForm :: Self -> Array JSX -> Array JSX
newAccountForm self children =
  Array.singleton $
    DOM.form
      { className: "vetrina--new-account-form"
      , onSubmit: handler preventDefault $ (\_ -> submitNewOrderForm self $ formValidations self)
      , children
      }

emailAddressInput :: Self -> JSX
emailAddressInput self@{ state: { form }} = InputField.inputField
  { type_: InputField.Email
  , label: "E-postadress"
  , name: "emailAddress"
  , placeholder: "E-postadress"
  , onChange: (\val -> self.setState _ { form { emailAddress = val
                                              -- If email value is changed, we must consider it as another
                                              -- attempt of creating a new account (it might be that
                                              -- an account with previous email exists, and we are
                                              -- asking the user to log in right now, so changing
                                              -- the email cancels that)
                                              , existingPassword = Nothing
                                              }
                                       -- Look comment about `existingPassword` above ^
                                       , accountStatus = NewAccount
                                       -- Clear server errors of EmailAddress when typing
                                       , serverErrors = Form.removeServerErrors EmailAddress self.state.serverErrors
                                       })
  , validationError: Form.inputFieldErrorMessage $ Form.validateField EmailAddress form.emailAddress self.state.serverErrors
  , value: form.emailAddress
  }

passwordInput :: Self -> JSX
passwordInput self = InputField.inputField
  { type_: InputField.Password
  , placeholder: "Lösenord"
  , label: "Lösenord"
  , name: "accountPassword"
  , value: Nothing
  , onChange: \pw -> self.setState _ { form { existingPassword = pw } }
  , validationError:
      Form.inputFieldErrorMessage $
      Form.validateField ExistingPassword self.state.form.existingPassword []
  }

setLoading :: Maybe Spinner.Loading -> State -> State
setLoading loading = _ { isLoading = loading }

submitNewOrderForm :: Self -> Form.ValidatedForm NewAccountInputField NewAccountForm -> Effect Unit
submitNewOrderForm self@{ state: { form, logger } } = unV
  (\errors -> self.setState _ { form { emailAddress = form.emailAddress <|> Just "" } })
  (\validForm -> Aff.launchAff_ $ Spinner.withSpinner (self.setState <<< setLoading) do
      eitherRes <- runExceptT do
        user <- ExceptT $ case self.state.accountStatus of
          NewAccount      -> createNewAccount self validForm.emailAddress
          ExistingAccount -> loginToExistingAccount self validForm.emailAddress validForm.existingPassword
        ExceptT $ Right unit <$ (liftEffect $ logger.setUser $ Just user)
        order      <- ExceptT $ createOrder user validForm.productSelection
        paymentUrl <- ExceptT $ payOrder order self.state.form.paymentMethod
        pure { paymentUrl, order, user }
      case eitherRes of
        Right { paymentUrl, order, user } ->
          liftEffect do
            self.setState _
              { purchaseState = CapturePayment paymentUrl
              , newOrder      = Just order
              , user          = Just user
              , orderFailure  = Nothing
              }
            startOrderPoller self order
        Left err
          | UnrecognizedError e <- err ->
            liftEffect do
              logger.error $ Error.orderError $ "Failed to place an order: " <> e
              self.setState _ { purchaseState = PurchaseFailed }
          | EmailInUse <- err -> liftEffect $ self.setState _ { accountStatus = ExistingAccount, orderFailure = Just EmailInUse }
          | otherwise -> liftEffect $ self.setState _ { orderFailure = Just err }
  )

createNewAccount :: Self -> Maybe String -> Aff (Either OrderFailure User)
createNewAccount self@{ state: { logger } } (Just emailString) = do
  newUser <- User.createUserWithEmail (User.Email emailString)
  case newUser of
    Right user -> do
      liftEffect $ self.setState _ { user = Just user }
      pure $ Right user
    Left User.RegistrationEmailInUse -> pure $ Left EmailInUse
    Left (User.InvalidFormFields errors) -> pure $ Left $ UnrecognizedError "invalid form fields"
    _ -> pure $ Left $ UnrecognizedError "Could not create a new account"

createNewAccount _ Nothing = pure $ Left $ UnrecognizedError ""

loginToExistingAccount :: Self -> Maybe String -> Maybe String -> Aff (Either OrderFailure User)
loginToExistingAccount self (Just username) (Just password) = do
  let login = { username, password, mergeToken: toNullable Nothing }
  eitherUser <- User.loginTraditional login
  case eitherUser of
    Right u  -> pure $ Right u
    Left err
      | User.LoginInvalidCredentials <- err -> pure $ Left AuthenticationError
      -- TODO: This is needed when we have magic login enabled
      | User.LoginTokenInvalid <- err -> pure $ Left $ UnrecognizedError "login token invalid"
      -- TODO: Think about this
      | User.InvalidFormFields _ <- err -> pure $ Left $ UnrecognizedError "invalid form fields"
      | User.SomethingWentWrong <- err -> pure $ Left $ ServerError
      | User.UnexpectedError jsError <- err -> do
        liftEffect $ self.state.logger.error $ Error.loginError $ message jsError
        pure $ Left $ ServerError
      | otherwise -> pure $ Left $ UnrecognizedError ""

loginToExistingAccount _ _ _ =
  pure $ Left $ FormFieldError [ EmailAddress, ExistingPassword ]


createOrder :: User -> Maybe Product -> Aff (Either OrderFailure Order)
createOrder user (Just product) = do
  -- TODO: fix period etc.
  let newOrder = { packageId: product.id, period: 1, payAmountCents: ceil $ product.price * 100.0 }
  eitherOrder <- User.createOrder newOrder
  pure $ case eitherOrder of
    Right order -> Right order
    Left err    -> Left $ UnrecognizedError err
createOrder _ Nothing =
  pure $ Left $ UnrecognizedError "Tried to create order with no order."

payOrder :: Order -> PaymentMethod -> Aff (Either OrderFailure PaymentTerminalUrl)
payOrder order paymentMethod =
  User.payOrder order.number paymentMethod >>= \eitherUrl ->
    pure $ case eitherUrl of
      Right url -> Right url
      Left err  -> Left $ UnrecognizedError err

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
  , existingPassword: _
  , productSelection: form.productSelection
  , paymentMethod: form.paymentMethod
  }
  <$> Form.validateField EmailAddress form.emailAddress []
  <*> (case self.state.accountStatus of
            ExistingAccount -> Form.validateField ExistingPassword form.existingPassword []
            -- If NewAccount, we don't need to validate the password field
            NewAccount      -> pure form.existingPassword)

netsTerminalIframe :: PaymentTerminalUrl -> JSX
netsTerminalIframe { paymentTerminalUrl } =
  DOM.iframe
    { src: paymentTerminalUrl
    , className: "vetrina--payment-terminal"
    }
