module Vetrina.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (all, any, snoc)
import Data.Array as Array
import Data.Either (Either(..), hush, isRight, note)
import Data.Foldable (foldMap)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Nullable (toNullable)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (toEither, unV)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import KSF.Api (InvalidateCache(..))
import KSF.Api.Package (PackageName(..), PackageValidationError(..), Package)
import KSF.Api.Package as Package
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
import React.Basic.Events (handler, handler_)
import Vetrina.Purchase.Completed as PurchaseCompleted

foreign import sentryDsn_ :: Effect String

type Props = { onClose :: Effect Unit }

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
  | PurchaseSubscriptionExists
  | PurchaseUnexpectedError
derive instance eqPurchaseState :: Eq PurchaseState

data NewAccountInputField
  = EmailAddress
  | ExistingPassword
  | ProductSelection
derive instance eqNewAccountInputField :: Eq NewAccountInputField
instance validatableFieldNewAccountInputField :: Form.ValidatableField NewAccountInputField where
  validateField field value serverErrors = case field of
    EmailAddress     -> Form.validateWithServerErrors serverErrors EmailAddress value Form.validateEmailAddress
    ExistingPassword -> Form.validateEmptyField ExistingPassword "Lösenord krävs." value
    ProductSelection -> Form.validateEmptyField ProductSelection "Produkt krävs." value

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
  | SubscriptionExists
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
  -- Before rendering the form, we need to:
  -- 1. fetch packages from the server, so we can actually show things to purchase
  -- 2. fetch the user if access token is found in the browser
  Aff.launchAff_ do
    Aff.finally
      -- When packages have been set (and user fetched), hide loading spinner
      (liftEffect $ self.setState \s -> s { isLoading = Nothing })
      do
        -- Try to login with local storage information and set user to state
        User.magicLogin (Just InvalidateCache) $ hush >>> \maybeUser -> self.setState _ { user = maybeUser }

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
            , maybe (emailAddressInput self) showLoggedInAccount self.state.user
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
    PurchaseDone ->
      PurchaseCompleted.completed
        -- TODO: The onError callback is invoked if setting the new password fails.
        -- We should think how to handle this. Probably we don't want to
        -- show an ORDER FAILED message, but rather just inform the user that
        -- something went wrong and please try to set the password again some other time.
        { onError: \_ -> pure unit
        , onComplete: self.props.onClose
        , user: self.state.user
        , logger: self.state.logger
        }
    PurchaseSubscriptionExists ->
      DOM.div_
        -- TODO: Waiting for copy
        [ DOM.text "You already have this subscription. Go back to article"
        , DOM.button
            { onClick: handler_ do
                 self.setState _ { purchaseState = NewPurchase }
                 self.props.onClose
            , children: [ DOM.text "OK" ]
            }
        ]

    PurchaseUnexpectedError -> DOM.text "SOMETHING WENT HORRIBLY WRONG SERVER SIDE"

orderErrorMessage :: OrderFailure -> JSX
orderErrorMessage failure =
  case failure of
    AuthenticationError -> InputField.errorMessage "Kombinationen av e-postadress och lösenord finns inte"
    EmailInUse -> DOM.text "Email already exists, please log in" -- TODO: Waiting for copy
    _ -> DOM.text "Något gick fel. Vänligen försök om en stund igen."

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

-- TODO: Waiting for copy
showLoggedInAccount :: User.User -> JSX
showLoggedInAccount user = DOM.text $ "Logged in as " <> user.email

-- TODO: Show forgot password link
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
        -- If user is found in state, clearly they already have an accout and are logged in
        user <- ExceptT $ case self.state.user of
          Just u  -> pure $ Right u
          Nothing -> case self.state.accountStatus of
            NewAccount      -> createNewAccount self validForm.emailAddress
            ExistingAccount -> loginToExistingAccount self validForm.emailAddress validForm.existingPassword
        ExceptT $ Right unit <$ (liftEffect $ logger.setUser $ Just user)
        product    <- ExceptT $ pure $ note (FormFieldError [ ProductSelection ]) self.state.form.productSelection
        when (userHasPackage product.packageName $ map _.package user.subs)
          $ ExceptT $ pure $ Left SubscriptionExists
        order      <- ExceptT $ createOrder user product
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
          | SubscriptionExists <- err -> liftEffect $ self.setState _ { purchaseState = PurchaseSubscriptionExists }
          | otherwise -> liftEffect $ self.setState _ { orderFailure = Just err }
  )

userHasPackage :: PackageName -> Array Package -> Boolean
userHasPackage packageName = isRight <<< Package.findPackage packageName

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
      -- TODO: Think about this
      | User.InvalidFormFields _ <- err -> pure $ Left $ UnrecognizedError "invalid form fields"
      | User.SomethingWentWrong <- err -> pure $ Left $ ServerError
      | User.UnexpectedError jsError <- err -> do
        liftEffect $ self.state.logger.error $ Error.loginError $ message jsError
        pure $ Left $ ServerError
      | otherwise -> pure $ Left $ UnrecognizedError ""
loginToExistingAccount _ _ _ =
  pure $ Left $ FormFieldError [ EmailAddress, ExistingPassword ]


createOrder :: User -> Product -> Aff (Either OrderFailure Order)
createOrder user product = do
  -- TODO: fix period etc.
  let newOrder = { packageId: product.id, period: 1, payAmountCents: ceil $ product.price * 100.0 }
  eitherOrder <- User.createOrder newOrder
  pure $ case eitherOrder of
    Right order -> Right order
    Left err    -> Left $ UnrecognizedError err

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
  <$> (if isNothing self.state.user
       then Form.validateField EmailAddress form.emailAddress []
       -- If User is already set, we don't care about the email input
       else pure form.emailAddress)
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
