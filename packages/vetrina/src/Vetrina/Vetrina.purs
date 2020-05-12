module KSF.Vetrina where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Except.Trans (except)
import Data.Array (head, length, mapMaybe, null)
import Data.Array as Array
import Data.Either (Either(..), either, hush, note)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import KSF.Api (InvalidateCache(..))
import KSF.Api.Package (Package, PackageId)
import KSF.InputField.Component as InputField
import KSF.JSError as Error
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User (Order, OrderStatusFailReason(..), OrderStatusState(..), PaymentMethod(..), PaymentTerminalUrl, User)
import KSF.User as User
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import Record (merge)
import Vetrina.Purchase.Completed as Purchase.Completed
import Vetrina.Purchase.Error as Purchase.Error
import Vetrina.Purchase.NewPurchase (FormInputField(..))
import Vetrina.Purchase.NewPurchase as NewPurchase
import Vetrina.Purchase.NewPurchase as Purchase.NewPurchase
import Vetrina.Purchase.SetPassword as Purchase.SetPassword
import Vetrina.Purchase.SubscriptionExists as Purchase.SubscriptionExists
import Vetrina.Types (AccountStatus(..), JSProduct, Product, fromJSProduct)

foreign import sentryDsn_ :: Effect String

type JSProps =
  { onClose :: Nullable (Effect Unit)
  , onLogin :: Nullable (Effect Unit)
  , products :: Nullable (Array JSProduct)
  }

type Props =
  { onClose  :: Effect Unit
  , onLogin  :: Effect Unit
  , products :: Either Error (Array Product)
  }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { onClose: fromMaybe (pure unit) $ toMaybe jsProps.onClose
  , onLogin: fromMaybe (pure unit) $ toMaybe jsProps.onLogin
  , products:
      let productError = error "Did not get any valid products in props!"
      in case toMaybe jsProps.products of
          Just jsProducts
            | products <- mapMaybe fromJSProduct jsProducts
            , not null products -> Right products
            | otherwise -> Left productError
          Nothing -> Left productError
  }

type State =
  { user          :: Maybe User
  , purchaseState :: PurchaseState
  , poller        :: Aff.Fiber Unit
  , isLoading     :: Maybe Spinner.Loading
  , accountStatus :: AccountStatus
  , logger        :: Sentry.Logger
  , products      :: Array Product
  , productSelection :: Maybe Product
  , paymentMethod :: User.PaymentMethod
  }

type Self = React.Self Props State

type SetState = (State -> State) -> Effect Unit

type PrevState = { prevProps :: Props, prevState :: State }

data PurchaseState
  = NewPurchase
  | CapturePayment PaymentTerminalUrl
  | ProcessPayment
  | PurchaseFailed (Maybe OrderFailure)
  | PurchaseSetPassword
  | PurchaseCompleted AccountStatus

data OrderFailure
  = EmailInUse String
  | SubscriptionExists
  | FormFieldError (Array NewPurchase.FormInputField)
  | AuthenticationError
  | ServerError
  | UnrecognizedError String
  | UnexpectedError

component :: React.Component Props
component = React.createComponent "Vetrina"

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { initialState, render, didMount }

vetrina :: Props -> JSX
vetrina = make component
  { initialState
  , render
  , didMount
  }

initialState :: State
initialState =
  { user: Nothing
  , purchaseState: NewPurchase
  , poller: pure unit
  , isLoading: Just Spinner.Loading -- Let's show spinner until user logged in
  , accountStatus: NewAccount
  , logger: Sentry.emptyLogger
  , products: []
  , productSelection: Nothing
  , paymentMethod: CreditCard
  }

didMount :: Self -> Effect Unit
didMount self = do
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing
  self.setState _ { logger = logger }
  -- Before rendering the form, we need to:
  -- 1. fetch the user if access token is found in the browser
  Aff.launchAff_ do
    Aff.finally
      -- When user has been fetched, hide loading spinner
      (liftEffect $ self.setState \s -> s { isLoading = Nothing })
      do
        -- Try to login with local storage information and set user to state
        tryMagicLogin self

        products <- liftEffect $ case self.props.products of
          Right p -> pure p
          Left err -> do
            self.setState _ { purchaseState = PurchaseFailed $ Just UnexpectedError }
            logger.error err
            throwError err

        liftEffect $ self.setState _ { products = products }
        -- If there is only one product given, automatically select that for the customer
        when (length products == 1) $
          liftEffect $ self.setState _ { productSelection = head products }

tryMagicLogin :: Self -> Aff Unit
tryMagicLogin self =
  User.magicLogin (Just InvalidateCache) $ hush >>> \maybeUser ->
    let newState = case maybeUser of
         Just user -> self.state { user = maybeUser, accountStatus = LoggedInAccount user }
         Nothing   -> self.state { user = maybeUser }
    in self.setState \_ -> newState

didUpdate :: Self -> PrevState -> Effect Unit
didUpdate self _ = Aff.launchAff_ $ stopOrderPollerOnCompletedState self

stopOrderPollerOnCompletedState :: Self -> Aff Unit
stopOrderPollerOnCompletedState self =
  case self.state.purchaseState of
    PurchaseFailed _    -> killOrderPoller self.state
    PurchaseCompleted _ -> killOrderPoller self.state
    NewPurchase         -> killOrderPoller self.state
    _                   -> pure unit

killOrderPoller :: State -> Aff Unit
killOrderPoller state = Aff.killFiber (error "Canceled poller") state.poller

startOrderPoller :: SetState -> State -> Order -> Effect Unit
startOrderPoller setState state order = do
  newPoller <- Aff.launchAff do
        killOrderPoller state
        newPoller <- Aff.forkAff $ pollOrder setState state (Right order)
        Aff.joinFiber newPoller
  setState _ { poller = newPoller }

pollOrder :: SetState -> State -> Either String Order -> Aff Unit
pollOrder setState state@{ logger } (Right order) = do
  Aff.delay $ Aff.Milliseconds 1000.0
  case order.status.state of
    OrderStarted -> do
      liftEffect $ setState _ { purchaseState = ProcessPayment }
      pollOrder setState state =<< User.getOrder order.number
    OrderCompleted -> do
      let userAccountStatus = maybe NewAccount chooseAccountStatus state.user
          -- If new user, show set password form. Otherwise we're done.
          nextPurchaseStep = case userAccountStatus of
            NewAccount      -> PurchaseSetPassword
            _               -> PurchaseCompleted userAccountStatus
      liftEffect $ setState _ { purchaseState = nextPurchaseStep }
      where
        chooseAccountStatus user
          | user.hasCompletedRegistration = ExistingAccount user.email
          | otherwise = NewAccount
    OrderFailed reason -> liftEffect do
      case reason of
        SubscriptionExistsError -> do
          logger.log "Tried to make purchase to with already existing subscription" Sentry.Info
          setState _ { purchaseState = PurchaseSubscriptionExists }
        _ -> do
          logger.error $ Error.orderError ("Order failed for customer: " <> show reason)
          setState _ { purchaseState = PurchaseFailed }
    OrderCanceled  -> liftEffect do
      logger.log "Customer canceled order" Sentry.Info
      setState _ { purchaseState = NewPurchase }
    OrderCreated   -> pollOrder setState state =<< User.getOrder order.number
    UnknownState   -> liftEffect do
      logger.error $ Error.orderError "Got UnknownState from server"
      setState _ { purchaseState = PurchaseFailed Nothing }
pollOrder setState { logger } (Left err) = liftEffect do
  logger.error $ Error.orderError $ "Failed to get order from server: " <> err
  setState _ { purchaseState = PurchaseFailed $ Just ServerError }

render :: Self -> JSX
render self = vetrinaContainer self $
  if isJust self.state.isLoading
  then Spinner.loadingSpinner
  else case self.state.purchaseState of
    NewPurchase ->
      Purchase.NewPurchase.newPurchase
        { accountStatus: self.state.accountStatus
        , products: self.state.products
        , errorMessage : Nothing
        , mkPurchaseWithNewAccount: mkPurchaseWithNewAccount self
        , mkPurchaseWithExistingAccount: mkPurchaseWithExistingAccount self
        , mkPurchaseWithLoggedInAccount: mkPurchaseWithLoggedInAccount self
        , paymentMethod: self.state.paymentMethod
        , productSelection: self.state.productSelection
        , onLogin: self.props.onLogin
        }
    CapturePayment url -> netsTerminalIframe url
    ProcessPayment -> Spinner.loadingSpinner
    PurchaseFailed failure ->
      case failure of
        Just SubscriptionExists ->
          DOM.div_
            -- TODO: Waiting for copy
            [ DOM.text "You already have this subscription. Go back to article"
            , DOM.button
                { onClick: handler_ self.props.onClose
                , children: [ DOM.text "OK" ]
                }
            ]
        Just AuthenticationError ->
          Purchase.NewPurchase.newPurchase
            { accountStatus: self.state.accountStatus
            , products: self.state.products
            , errorMessage: Just $ orderErrorMessage AuthenticationError
            , mkPurchaseWithNewAccount: mkPurchaseWithNewAccount self
            , mkPurchaseWithExistingAccount: mkPurchaseWithExistingAccount self
            , mkPurchaseWithLoggedInAccount: mkPurchaseWithLoggedInAccount self
            , paymentMethod: self.state.paymentMethod
            , productSelection: self.state.productSelection
            , onLogin: self.props.onLogin
            }
        _ ->
          Purchase.Error.error
            { onRetry: onRetry
            }
    PurchaseSetPassword ->
      Purchase.SetPassword.setPassword
        -- TODO: The onError callback is invoked if setting the new password fails.
        -- We should think how to handle this. Probably we don't want to
        -- show an ORDER FAILED message, but rather just inform the user that
        -- something went wrong and please try to set the password again some other time.
        { onError: \_ -> pure unit
        , onSuccess: self.setState _ { purchaseState = PurchaseCompleted NewAccount }
        , user: self.state.user
        , logger: self.state.logger
        }
    PurchaseCompleted accountStatus ->
      Purchase.Completed.completed
        { onClose: self.props.onClose
        , user: self.state.user
        , accountStatus
        }
  where
    onRetry = self.setState _ { purchaseState = NewPurchase }

vetrinaContainer :: Self -> JSX -> JSX
vetrinaContainer self@{ state: { purchaseState } } child =
  let errorClassString = "vetrina--purchase-error"
      errorClass       = case purchaseState of
                           PurchaseFailed (Just SubscriptionExists)  -> mempty
                           PurchaseFailed (Just AuthenticationError) -> mempty
                           PurchaseFailed _                          -> errorClassString
                           otherwise                                 -> mempty
  in
    DOM.div
      { className: "vetrina--container " <> errorClass
      , children: [ child ]
      }

orderErrorMessage :: OrderFailure -> String
orderErrorMessage failure =
  case failure of
    AuthenticationError -> "Kombinationen av e-postadress och lösenord finns inte"
    _                   -> "Något gick fel. Vänligen försök om en stund igen."

-- TODO: Validate `acceptLegalTerms` of `NewAccountForm`
mkPurchaseWithNewAccount :: Self -> NewPurchase.NewAccountForm -> Effect Unit
mkPurchaseWithNewAccount self validForm = mkPurchase self validForm $ createNewAccount self validForm.emailAddress

mkPurchaseWithExistingAccount :: Self -> NewPurchase.ExistingAccountForm -> Effect Unit
mkPurchaseWithExistingAccount self validForm =
  mkPurchase self validForm $ loginToExistingAccount self validForm.emailAddress validForm.password

mkPurchaseWithLoggedInAccount :: Self -> User.User -> { | NewPurchase.PurchaseParameters } -> Effect Unit
mkPurchaseWithLoggedInAccount self user validForm = mkPurchase self validForm $ (pure $ Right user)

mkPurchase
  :: forall r
  . Self
  -> { productSelection :: Maybe Product, paymentMethod :: Maybe PaymentMethod | r }
  -> Aff (Either OrderFailure User.User)
  -> Effect Unit
mkPurchase self@{ state: { logger } } validForm affUser = Aff.launchAff_ $ Spinner.withSpinner (self.setState <<< Spinner.setSpinner) do
  eitherUser <- affUser
  eitherOrder <- runExceptT do
    user          <- except eitherUser
    product       <- except $ note (FormFieldError [ ProductSelection ]) validForm.productSelection
    paymentMethod <- except $ note (FormFieldError [ PaymentMethod ])    validForm.paymentMethod

    when (userHasPackage product.id $ map _.package user.subs)
      $ except $ Left SubscriptionExists

    order <- ExceptT $ createOrder user product
    paymentUrl <- ExceptT $ payOrder order paymentMethod
    pure { paymentUrl, order }
  case eitherOrder of
    Right { paymentUrl, order } ->
      liftEffect do
        let newState =
              self.state { purchaseState = CapturePayment paymentUrl
                         , user = hush eitherUser
                         , accountStatus = newAccountStatus
                         }
            newAccountStatus = either (const NewAccount) LoggedInAccount eitherUser
        self.setState \_ -> newState
        -- NOTE: We need to pass the updated state here, not `self.state`.
        startOrderPoller self.setState newState order
    Left err -> do
      case err of
        UnrecognizedError e -> liftEffect $ logger.error $ Error.orderError $ "Failed to place an order: " <> e
        _ -> pure unit

      let errState = { user: hush eitherUser } `merge` case err of
            emailInUse@(EmailInUse email) -> self.state { accountStatus = ExistingAccount email }
            SubscriptionExists            -> self.state { purchaseState = PurchaseSubscriptionExists }
            -- TODO: Handle all cases explicitly
            _                             -> self.state { purchaseState = PurchaseFailed }
      liftEffect $ self.setState \_ -> errState

userHasPackage :: PackageId -> Array Package -> Boolean
userHasPackage packageId = Array.any (\p -> packageId == p.id)

createNewAccount :: Self -> Maybe String -> Aff (Either OrderFailure User)
createNewAccount self@{ state: { logger } } (Just emailString) = do
  nowISO <- liftEffect $ JSDate.toISOString =<< JSDate.now
  let legalConsent =
        { consentId: "legal_acceptance_v1"
        , screenName: "legalAcceptanceScreen"
        , dateAccepted: nowISO
        }
  newUser <- User.createUserWithEmail { emailAddress: User.Email emailString, legalConsents: [ legalConsent ] }
  case newUser of
    Right user -> pure $ Right user
    Left User.RegistrationEmailInUse -> pure $ Left $ EmailInUse emailString
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
  pure $ Left $ FormFieldError [ EmailAddress, Password ]

createOrder :: User -> Product -> Aff (Either OrderFailure Order)
createOrder user product = do
  -- TODO: fix period etc.
  let newOrder = { packageId: product.id, period: 1, payAmountCents: product.priceCents }
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

netsTerminalIframe :: PaymentTerminalUrl -> JSX
netsTerminalIframe { paymentTerminalUrl } =
  DOM.iframe
    { src: paymentTerminalUrl
    , className: "vetrina--payment-terminal"
    }
