module KSF.Vetrina where

import Prelude

import Bottega (BottegaError(..))
import Bottega.Models.PaymentMethod (toPaymentMethod)
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Except.Trans (except)
import Data.Array (filter, head, length, mapMaybe, null, take)
import Data.Array as Array
import Data.Either (Either(..), either, hush, note)
import Data.Foldable (foldMap)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import KSF.Api (InvalidateCache(..))
import KSF.Api.Package (Package, PackageId)
import KSF.JSError as Error
import KSF.LocalStorage as LocalStorage
import KSF.Paper (Paper)
import KSF.Paper as Paper
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User (Order, FailReason(..), OrderState(..), PaymentMethod(..), PaymentTerminalUrl, User)
import KSF.User as User
import React.Basic.Classic (JSX, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import Record (merge)
import Tracking as Tracking
import Vetrina.Purchase.AccountForm (mkAccountForm)
import Vetrina.Purchase.AccountForm as AccountForm
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
  { onClose            :: Nullable (Effect Unit)
  , onLogin            :: Nullable (Effect Unit)
  , products           :: Nullable (Array JSProduct)
  , unexpectedError    :: Nullable JSX
  , accessEntitlements :: Nullable (Array String)
  , headline           :: Nullable JSX
  , paper              :: Nullable String
  , paymentMethods     :: Nullable (Array String)
  , minimalLayout      :: Nullable Boolean
  }

type Props =
   -- If onClose is Nothing, the final button in `Completed` view will not be shown
  { onClose            :: Maybe (Effect Unit)
  , onLogin            :: Effect Unit
  , products           :: Either Error (Array Product)
  , unexpectedError    :: JSX
  , accessEntitlements :: Set String
  , headline           :: Maybe JSX
  , paper              :: Maybe Paper
  , paymentMethods     :: Array User.PaymentMethod
  -- If true, will show texts, links etc.
  -- FIXME: This is nothing but a band-aid and the entire thing should be redesigned
  , minimalLayout      :: Boolean
  }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { onClose: toMaybe jsProps.onClose
  , onLogin: fromMaybe (pure unit) $ toMaybe jsProps.onLogin
  , products:
      let productError = error "Did not get any valid products in props!"
      in case toMaybe jsProps.products of
          Just jsProducts
            | products <- mapMaybe fromJSProduct jsProducts
            , not null products -> Right products
            | otherwise -> Left productError
          Nothing -> Left productError
  , unexpectedError: fromMaybe mempty $ toMaybe jsProps.unexpectedError
  , accessEntitlements: maybe Set.empty Set.fromFoldable $ toMaybe jsProps.accessEntitlements
  , headline: toMaybe jsProps.headline
  , paper: Paper.fromString =<< toMaybe jsProps.paper
  , paymentMethods: foldMap (mapMaybe toPaymentMethod) $ toMaybe jsProps.paymentMethods
  , minimalLayout: fromMaybe false $ toMaybe jsProps.minimalLayout
  }

type State =
  { user             :: Maybe User
  , purchaseState    :: PurchaseState
  , poller           :: Aff.Fiber Unit
  , isLoading        :: Maybe Spinner.Loading
  , loadingMessage   :: Maybe String
  , accountStatus    :: AccountStatus
  , logger           :: Sentry.Logger
  , products         :: Array Product
  , productSelection :: Maybe Product
  , paymentMethod    :: Maybe User.PaymentMethod
  , accountFormComponent :: AccountForm.Props -> JSX
  , retryPurchase :: User -> Effect Unit
  , paymentMethods :: Array User.PaymentMethod
  }

type Self = React.Self Props State

type SetState = (State -> State) -> Effect Unit

type PrevState = { prevProps :: Props, prevState :: State }

data PurchaseState
  = NewPurchase
  | CapturePayment PaymentTerminalUrl
  | ProcessPayment
  | PurchaseFailed OrderFailure
  | PurchaseSetPassword
  | PurchaseCompleted AccountStatus

data OrderFailure
  = EmailInUse String
  | SubscriptionExists
  | InsufficientAccount
  | InitializationError
  | FormFieldError (Array NewPurchase.FormInputField)
  | AuthenticationError
  | ServerError
  | UnexpectedError String

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
  , loadingMessage: Nothing
  , accountStatus: NewAccount
  , logger: Sentry.emptyLogger
  , products: []
  , productSelection: Nothing
  , paymentMethod: Nothing
  , accountFormComponent: const mempty
  , retryPurchase: const $ pure unit
  , paymentMethods: mempty
  }

didMount :: Self -> Effect Unit
didMount self = do
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "vetrina"
  accountFormComponent <- mkAccountForm
  let paymentMethods =
        if null self.props.paymentMethods
        then [ User.CreditCard ]
        else self.props.paymentMethods
      paymentMethod =
        if length paymentMethods == 1
        then head paymentMethods
        else Nothing
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
            self.setState _ { purchaseState = PurchaseFailed InitializationError }
            logger.error err
            throwError err

        liftEffect $ self.setState _
          { products = products
          , accountFormComponent = accountFormComponent
          , logger = logger
          , paymentMethods = paymentMethods
          , paymentMethod = paymentMethod
          }

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

pollOrder :: SetState -> State -> Either BottegaError Order -> Aff Unit
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
      liftEffect do
        setState _ { purchaseState = nextPurchaseStep }
        productId         <- LocalStorage.getItem "productId" -- analytics
        productPrice      <- LocalStorage.getItem "productPrice" -- analytics
        productCampaignNo <- LocalStorage.getItem "productCampaignNo" -- analytics
        Tracking.transaction order.number productId productPrice productCampaignNo -- analyics
      where
        chooseAccountStatus user
          | user.hasCompletedRegistration = ExistingAccount user.email
          | otherwise = NewAccount
    OrderFailed reason -> liftEffect do
      case reason of
        SubscriptionExistsError -> do
          logger.log "Tried to make purchase to with already existing subscription" Sentry.Info
          setState _ { purchaseState = PurchaseFailed SubscriptionExists }
        _ -> do
          logger.error $ Error.orderError ("Order failed for customer: " <> show reason)
          setState _ { purchaseState = PurchaseFailed $ UnexpectedError "" }
    OrderCanceled     -> liftEffect do
      logger.log "Customer canceled order" Sentry.Info
      setState _ { purchaseState = NewPurchase }
    OrderCreated      -> pollOrder setState state =<< User.getOrder order.number
    OrderUnknownState -> liftEffect do
      logger.error $ Error.orderError "Got UnknownState from server"
      setState _ { purchaseState = PurchaseFailed ServerError }
pollOrder setState { logger } (Left bottegaErr) = liftEffect do
  let errMessage = case bottegaErr of
        BottegaUnexpectedError e   -> e
        BottegaInsufficientAccount -> "InsufficientAccount"
  logger.error $ Error.orderError $ "Failed to get order from server: " <> errMessage
  setState _ { purchaseState = PurchaseFailed ServerError }

render :: Self -> JSX
render self = vetrinaContainer self $
  if isJust self.state.isLoading
  then maybe Spinner.loadingSpinner Spinner.loadingSpinnerWithMessage self.state.loadingMessage
  else case self.state.purchaseState of
    NewPurchase ->
      Purchase.NewPurchase.newPurchase
        { accountStatus: self.state.accountStatus
        , products: self.state.products
        , errorMessage : Nothing
        , mkPurchaseWithNewAccount: mkPurchaseWithNewAccount self
        , mkPurchaseWithExistingAccount: mkPurchaseWithExistingAccount self
        , mkPurchaseWithLoggedInAccount: mkPurchaseWithLoggedInAccount self
        , productSelection: self.state.productSelection
        , onLogin: self.props.onLogin
        , headline: self.props.headline
        , paper: self.props.paper
        , paymentMethod: self.state.paymentMethod
        , paymentMethods: self.state.paymentMethods
        , onPaymentMethodChange: \p -> self.setState _ { paymentMethod = p }
        , minimalLayout: self.props.minimalLayout
        }
    CapturePayment url -> netsTerminalIframe url
    ProcessPayment -> Spinner.loadingSpinner
    PurchaseFailed failure ->
      case failure of
        SubscriptionExists ->
          Purchase.SubscriptionExists.subscriptionExists
            { onClose: fromMaybe (pure unit) self.props.onClose }
        InsufficientAccount ->
          case self.state.user of
            Just u ->
              self.state.accountFormComponent
                { user: u
                , retryPurchase: self.state.retryPurchase
                , setLoading: \loading -> self.setState _ { isLoading = loading }
                }
            -- Can't do much without a user
            Nothing -> self.props.unexpectedError
        AuthenticationError ->
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
            , headline: self.props.headline
            , paper: self.props.paper
            , paymentMethods: self.state.paymentMethods
            , onPaymentMethodChange: \p -> self.setState _ { paymentMethod = p }
            , minimalLayout: self.props.minimalLayout
            }
        ServerError ->
          Purchase.Error.error
            { onRetry: onRetry
            }
        UnexpectedError _ ->
          Purchase.Error.error
            { onRetry: onRetry
            }
        InitializationError ->
          self.props.unexpectedError
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
        , purchasedProduct: self.state.productSelection
        }
  where
    onRetry = self.setState _ { purchaseState = NewPurchase }

vetrinaContainer :: Self -> JSX -> JSX
vetrinaContainer self@{ state: { purchaseState } } child =
  let errorClassString = "vetrina--purchase-error"
      errorClass       = case purchaseState of
                           PurchaseFailed SubscriptionExists  -> mempty
                           PurchaseFailed AuthenticationError -> mempty
                           PurchaseFailed InsufficientAccount -> mempty
                           PurchaseFailed _                   -> errorClassString
                           otherwise                          -> mempty
      minimalClass = if self.props.minimalLayout then "vetrina--minimal-layout" else mempty
  in
    DOM.div
      { className:
          joinWith " "
          -- `errorClass` and `minimalClass` do not play well together
          -- Yeah this is not that good...
          -- TODO: Make a saner way of `minimalLayout`
          $ take 2
          $ filter (not String.null) [ "vetrina--container", errorClass, minimalClass ]
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
mkPurchase self@{ state: { logger } } validForm affUser =
  Aff.launchAff_ $ Spinner.withSpinner loadingWithMessage do
  eitherUser <- affUser
  eitherOrder <- runExceptT do

    -- TODO: We could check for insufficient account before trying to create the order and
    -- wait for the validation done server side. For this, we would need the packages from Bottega so
    -- that we can tell if a package id belongs to a paper product or not.
    -- Insufficient account = user not having contact information and trying to purchase a paper product
    user          <- except eitherUser
    product       <- except $ note (FormFieldError [ ProductSelection ]) validForm.productSelection
    paymentMethod <- except $ note (FormFieldError [ PaymentMethod ])    validForm.paymentMethod

    when (userHasPackage product.id $ map _.package user.subs)
      $ except $ Left SubscriptionExists

    userEntitlements <- ExceptT getUserEntitlements
    when (isUserEntitled self.props.accessEntitlements userEntitlements)
      $ except $ Left SubscriptionExists

    order <- ExceptT $ createOrder user product
    paymentUrl <- ExceptT $ payOrder order paymentMethod
    liftEffect do
      LocalStorage.setItem "productId" product.id -- for analytics
      LocalStorage.setItem "productPrice" $ show product.priceCents -- for analytics
      LocalStorage.setItem "productCampaingNo" $ foldMap show $ map _.no product.campaign

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
        UnexpectedError e -> liftEffect $ logger.error $ Error.orderError $ "Failed to place an order: " <> e
        _ -> pure unit

      let errState = { user: hush eitherUser } `merge` case err of
            emailInUse@(EmailInUse email) -> self.state { accountStatus = ExistingAccount email
                                                        , purchaseState = NewPurchase
                                                        -- Let's keep the selected product even when
                                                        -- asking for the password
                                                        , productSelection = validForm.productSelection
                                                        }
            SubscriptionExists            -> self.state { purchaseState = PurchaseFailed SubscriptionExists }
            AuthenticationError           -> self.state { purchaseState = PurchaseFailed AuthenticationError }
            InsufficientAccount           ->
              self.state { purchaseState = PurchaseFailed InsufficientAccount
                         , retryPurchase = \user ->
                             mkPurchase self validForm (pure $ Right user)
                         }
            -- TODO: Handle all cases explicitly
            _                             -> self.state { purchaseState = PurchaseFailed $ UnexpectedError "" }
      liftEffect $ self.setState \_ -> errState
  where
    loadingWithMessage spinner = self.setState _
        { isLoading = spinner
        , loadingMessage =
            case self.state.paymentMethod of
              Just CreditCard ->
                if isJust spinner
                then Just "Tack, vi skickar dig nu vidare till betalningsleverantören Nets."
                else Nothing
              _ -> Nothing
        }

userHasPackage :: PackageId -> Array Package -> Boolean
userHasPackage packageId = Array.any (\p -> packageId == p.id)

isUserEntitled :: Set String -> Set String -> Boolean
isUserEntitled accessEntitlements userEntitlements =
  not $ Set.isEmpty $ Set.intersection accessEntitlements userEntitlements

getUserEntitlements :: Aff (Either OrderFailure (Set String))
getUserEntitlements = do
  eitherEntitlements <- User.getUserEntitlementsLoadToken
  pure case eitherEntitlements of
    Right entitlements -> Right entitlements
    Left err -> Left (UnexpectedError $ show err)

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
    Left (User.InvalidFormFields errors) -> pure $ Left $ UnexpectedError "invalid form fields"
    _ -> pure $ Left $ UnexpectedError "Could not create a new account"
createNewAccount _ Nothing = pure $ Left $ UnexpectedError ""

loginToExistingAccount :: Self -> Maybe String -> Maybe String -> Aff (Either OrderFailure User)
loginToExistingAccount self (Just username) (Just password) = do
  let login = { username, password, mergeToken: toNullable Nothing }
  eitherUser <- User.loginTraditional login
  case eitherUser of
    Right u  -> pure $ Right u
    Left err
      | User.LoginInvalidCredentials <- err -> pure $ Left AuthenticationError
      -- TODO: Think about this
      | User.InvalidFormFields _ <- err -> pure $ Left $ UnexpectedError "invalid form fields"
      | User.SomethingWentWrong <- err -> pure $ Left $ ServerError
      | User.UnexpectedError jsError <- err -> do
        liftEffect $ self.state.logger.error $ Error.loginError $ message jsError
        pure $ Left $ ServerError
      | otherwise -> pure $ Left $ UnexpectedError ""
loginToExistingAccount _ _ _ =
  pure $ Left $ FormFieldError [ EmailAddress, Password ]

createOrder :: User -> Product -> Aff (Either OrderFailure Order)
createOrder user product = do
  -- TODO: fix period etc.
  let newOrder =
        { packageId: product.id
        , period: 1
        , payAmountCents: product.priceCents
        , campaignNo: map _.no product.campaign
        }
  eitherOrder <- User.createOrder newOrder
  pure $ case eitherOrder of
    Right order -> Right order
    Left err    -> Left $ toOrderFailure err

payOrder :: Order -> PaymentMethod -> Aff (Either OrderFailure PaymentTerminalUrl)
payOrder order paymentMethod =
  User.payOrder order.number paymentMethod >>= \eitherUrl ->
    pure $ case eitherUrl of
      Right url -> Right url
      Left err  -> Left $ toOrderFailure err

toOrderFailure :: BottegaError -> OrderFailure
toOrderFailure bottegaErr =
  case bottegaErr of
    BottegaInsufficientAccount    -> InsufficientAccount
    BottegaUnexpectedError errMsg -> UnexpectedError errMsg

netsTerminalIframe :: PaymentTerminalUrl -> JSX
netsTerminalIframe { paymentTerminalUrl } =
  DOM.div
    { className: "vetrina--payment-wrapper"
    , children:
      [ DOM.iframe
        { src: paymentTerminalUrl
        , className: "vetrina--payment-terminal"
        }
      ]
    }
