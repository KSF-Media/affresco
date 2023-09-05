module KSF.Vetrina where

import Prelude

import Bottega (BottegaError(..))
import Bottega.Models.Order (OrderSource(..))
import Control.Alt ((<|>))
import Data.Array (filter, head, length, take)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set (Set)
import Data.String (joinWith)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
import KSF.JSError as Error
import KSF.LocalStorage as LocalStorage
import KSF.Paper (Paper)
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User (Order, FailReason(..), OrderState(..), PaymentTerminalUrl, User)
import KSF.User as User
import React.Basic (JSX)
import React.Basic.Hooks (Component, useEffect, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (EventHandler, handler)
import Tracking as Tracking
import Vetrina.Purchase.AccountForm (mkAccountForm)
import Vetrina.Purchase.Completed as Purchase.Completed
import Vetrina.Purchase.Error as Purchase.Error
import Vetrina.Purchase.NewPurchase as Purchase.NewPurchase
import Vetrina.Purchase.SetPassword as Purchase.SetPassword
import Vetrina.Purchase.SubscriptionExists as Purchase.SubscriptionExists
import Vetrina.Types (AccountStatus(..), OrderFailure(..), Product, PurchaseState(..))
import Web.HTML as Web.HTML
import Web.HTML.Window as Window

foreign import sentryDsn_ :: Effect String
foreign import scrollToVetrina :: Effect Unit

type Props =
   -- If onClose is Nothing, the final button in `Completed` view will not be shown
  { onClose            :: Maybe (Effect Unit)
  , onLogin            :: EventHandler
  , user               :: Maybe User
  , setUser            :: User -> Effect Unit
  , products           :: Array Product
  , unexpectedError    :: JSX
  , accessEntitlements :: Set String
  , headline           :: Maybe JSX
  , paper              :: Maybe Paper
  , paymentMethods     :: Array User.PaymentMethod
  , customNewPurchase  :: Maybe (JSX -> AccountStatus -> JSX)
  , orderSource        :: OrderSource
  , subscriptionExists :: JSX
  , askAccountAlways   :: Boolean
  }

type State =
  { user             :: Maybe User
  , purchaseState    :: PurchaseState
  , poller           :: Aff.Fiber Unit
  , isLoading        :: Maybe Spinner.Loading
  , loadingMessage   :: Maybe String
  , accountStatus    :: AccountStatus
  , productSelection :: Maybe Product
  , paymentMethod    :: Maybe User.PaymentMethod
  , paymentTerminal  :: Maybe String
  , scaShown         :: Boolean
  , retryPurchase    :: User -> Effect Unit
  }

type SetState = (State -> State) -> Effect Unit

data Self = Self Props State SetState

type Components =
  { newPurchase :: JSX
  , accountForm :: User -> JSX
  , setPassword :: User -> JSX
  }

component :: Component Props
component = do
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "vetrina"
  accountFormComponent <- mkAccountForm
  newPurchaseComponent <- Purchase.NewPurchase.component logger
  setPasswordComponent <- Purchase.SetPassword.component
  React.component "Vetrina" $ \props -> React.do
    let paymentMethod =
          if length props.paymentMethods == 1
          then head props.paymentMethods
          else Nothing
    state /\ setState <- useState
      { user: props.user
      , purchaseState: NewPurchase
      , poller: pure unit
      , isLoading: Nothing
      , loadingMessage: Nothing
      , accountStatus: maybe NewAccount LoggedInAccount props.user
      , productSelection: Nothing
      , paymentMethod
      , paymentTerminal: Nothing
      , scaShown: false
      , retryPurchase: const $ pure unit
      }

    useEffect state.purchaseState $ do
      let stop = Aff.launchAff_ do
            liftEffect do
              scrollToVetrina
              setState _ { paymentTerminal = Nothing
                         , retryPurchase = const $ pure unit
                         }
            killOrderPoller state
      case state.purchaseState of
        PurchaseFailed _    -> stop
        PurchaseCompleted _ -> stop
        _                   -> pure unit
      pure $ Aff.launchAff_ $ killOrderPoller state

    let startOrder :: Maybe PaymentTerminalUrl -> Order -> User -> AccountStatus -> Effect Unit
        startOrder Nothing order user accountStatus = do
          let newState =
                state { purchaseState = PurchasePolling
                      , user = Just user
                      , accountStatus = accountStatus
                      , paymentTerminal = Nothing
                      }
          setState $ const newState
          startOrderPoller logger setState newState order
        startOrder (Just terminalUrl) order user accountStatus = do
          let newState =
                state { purchaseState = CapturePayment
                      , user = Just user
                      , accountStatus = accountStatus
                      , paymentTerminal = Just terminalUrl.paymentTerminalUrl
                      }
          setState $ const newState
          startOrderPoller logger setState newState order
        purchaseError maybeUser maybeAccountStatus maybeProductSelection purchaseState = do
          setState _ { user = maybeUser <|> state.user
                     , accountStatus = fromMaybe state.accountStatus maybeAccountStatus
                     , productSelection = maybeProductSelection <|> state.productSelection
                     , purchaseState = purchaseState
                     }
        loadingWithMessage isLoading loadingMessage =
          setState _ { isLoading = isLoading
                     , loadingMessage = loadingMessage
                     }
        setRetryPurchase f = setState _ { retryPurchase = f }

        -- Components
        newPurchase = newPurchaseComponent
          { accountStatus: maybe state.accountStatus LoggedInAccount state.user
          , products: props.products
          , askAccountAlways: props.askAccountAlways
          , accessEntitlements: props.accessEntitlements
          , startOrder
          , purchaseError
          , setRetryPurchase
          , setUser: props.setUser
          , loading: loadingWithMessage
          , productSelection: state.productSelection
          , onLogin: props.onLogin
          , headline: props.headline
          , paper: props.paper
          , paymentMethod: state.paymentMethod
          , paymentMethods: props.paymentMethods
          , onPaymentMethodChange: \p -> setState _ { paymentMethod = p }
          , onEmailChange: setState _ { accountStatus = NewAccount }
          , customRender: props.customNewPurchase
          , orderSource: props.orderSource
          }

        accountForm u = accountFormComponent
          { user: u
          , retryPurchase: \user -> do
              setState _ { purchaseState = PurchasePolling }
              state.retryPurchase user
          , setLoading: \loading -> setState _ { isLoading = loading }
          , onError: \userError -> do
            -- NOTE: The temporary user is already created at this point, but no passwords are given (impossible to login).
            -- The user is logged in the current session though, so it's recoverable.
              logger.error $ Error.orderError $ "Failed to update user: " <> show userError
              setState _ { purchaseState = PurchaseFailed $ UnexpectedError "" }
          }

        -- TODO: The onError callback is invoked if setting the new password fails.
        -- We should think how to handle this. Probably we don't want to
        -- show an ORDER FAILED message, but rather just inform the user that
        -- something went wrong and please try to set the password again some other time.
        setPassword user = setPasswordComponent
          { onError: \_ -> pure unit
          , onSuccess: setState _ { purchaseState = PurchaseCompleted NewAccount }
          , user
          , logger
          }

        components =
          { newPurchase
          , accountForm
          , setPassword
          }

    pure $ render (Self props state setState) components

killOrderPoller :: State -> Aff Unit
killOrderPoller state = Aff.killFiber (error "Canceled poller") state.poller

startOrderPoller :: Sentry.Logger -> SetState -> State -> Order -> Effect Unit
startOrderPoller logger setState state order = do
  newPoller <- Aff.launchAff do
        killOrderPoller state
        newPoller <- Aff.forkAff $ pollOrder logger setState state (Right order)
        Aff.joinFiber newPoller
  setState _ { poller = newPoller }

pollOrder :: Sentry.Logger -> SetState -> State -> Either BottegaError Order -> Aff Unit
pollOrder logger setState state (Right order) = do
  Aff.delay $ Aff.Milliseconds 1000.0
  case order.status.state of
    OrderStarted -> do
      liftEffect $ setState _ { purchaseState = ProcessPayment }
      pollOrder logger setState state =<< User.getOrder order.number
    OrderCompleted -> do
      let userAccountStatus = maybe NewAccount chooseAccountStatus state.user
          -- If new user, show set password form. Otherwise we're done.
          nextPurchaseStep = case userAccountStatus of
            NewAccount      -> PurchaseSetPassword
            _               -> PurchaseCompleted userAccountStatus
      liftEffect do
        scrollToVetrina
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
        NetsIssuerError -> do
          logger.log "Refused by issuer for customer" Sentry.Info
          setState _ { purchaseState = PurchaseFailed RefusedByIssuer }
        _ -> do
          logger.error $ Error.orderError ("Order failed for customer: " <> show reason)
          scrollToVetrina
          setState _ { purchaseState = PurchaseFailed $ UnexpectedError "" }
    OrderCanceled     -> liftEffect do
      logger.log "Customer canceled order" Sentry.Info
      scrollToVetrina
      setState _ { purchaseState = NewPurchase }
    OrderScaRequired -> do
      liftEffect $ setState _ { purchaseState = ScaRequired }
      pollOrder logger setState state =<< User.getOrder order.number
    OrderCreated      -> pollOrder logger setState state =<< User.getOrder order.number
    OrderUnknownState -> liftEffect do
      scrollToVetrina
      logger.error $ Error.orderError "Got UnknownState from server"
      setState _ { purchaseState = PurchaseFailed ServerError }
pollOrder logger setState _ (Left bottegaErr) = liftEffect do
  let errMessage = case bottegaErr of
        BottegaUnexpectedError e   -> e
        BottegaInsufficientAccount -> "InsufficientAccount"
        BottegaTimeout             -> "Timeout"
  scrollToVetrina
  logger.error $ Error.orderError $ "Failed to get order from server: " <> errMessage
  setState _ { purchaseState = PurchaseFailed ServerError }

render :: Self -> Components -> JSX
render (Self props state setState) components = vetrinaContainer state.purchaseState $
  if isJust state.isLoading
  then maybe Spinner.loadingSpinner Spinner.loadingSpinnerWithMessage state.loadingMessage
  else case state.purchaseState of
    PurchasePolling -> maybe Spinner.loadingSpinner Spinner.loadingSpinnerWithMessage state.loadingMessage
    NewPurchase -> components.newPurchase
    CapturePayment -> netsTerminalModal
    ScaRequired ->
        if state.scaShown
        then Spinner.loadingSpinner
        else scaRequired state.paymentTerminal (\scaShown -> setState _ { scaShown = scaShown })
    ProcessPayment -> Spinner.loadingSpinner
    PurchaseFailed failure ->
      case failure of
        SubscriptionExists ->
          Purchase.SubscriptionExists.subscriptionExists
            { onClose: fromMaybe (pure unit) props.onClose
            , extraMsg: props.subscriptionExists
            }
        InsufficientAccount ->
          case state.user of
            Just u ->
              components.accountForm u
            -- Can't do much without a user
            Nothing -> props.unexpectedError
        AuthenticationError -> components.newPurchase
        RefusedByIssuer     -> Purchase.Error.refusedByIssuer { onRetry }
        ServerError         -> Purchase.Error.error { onRetry }
        UnexpectedError _   -> Purchase.Error.error { onRetry }
        InitializationError -> props.unexpectedError
        _                   -> Purchase.Error.error { onRetry }
    PurchaseSetPassword ->
      case state.user of
        Just u -> components.setPassword u
        Nothing -> props.unexpectedError
    PurchaseCompleted accountStatus ->
      Purchase.Completed.completed
        { onClose: props.onClose
        , user: state.user
        , accountStatus
        , purchasedProduct: state.productSelection
        }
  where
    onRetry = setState _ { purchaseState = NewPurchase
                         , scaShown = false
                         }

vetrinaContainer :: PurchaseState -> JSX -> JSX
vetrinaContainer purchaseState child =
  let errorClassString = "vetrina--purchase-error"
      errorClass       = case purchaseState of
                           PurchaseFailed SubscriptionExists  -> mempty
                           PurchaseFailed AuthenticationError -> mempty
                           PurchaseFailed InsufficientAccount -> mempty
                           PurchaseFailed _                   -> errorClassString
                           _                                  -> mempty
  in
    DOM.div
      { className:
          joinWith " "
          $ take 3
          $ filter (not String.null) [ "vetrina--container", errorClass, purchaseStateClass ]
      , children: [ child ]
      }
  where
    purchaseStateClass = case purchaseState of
                        NewPurchase -> "vetrina--container-new-purchase"
                        PurchaseSetPassword -> "flex flex-col justify-center content-center px-5 pt-14 pb-10"
                        PurchaseCompleted _ -> "flex flex-col justify-center items-center content-center font-duplexsans font-light text-base px-5 pt-14 pb-10"
                        _           -> mempty

orderErrorMessage :: OrderFailure -> String
orderErrorMessage failure =
  case failure of
    AuthenticationError -> "Kombinationen av e-postadress och lösenord finns inte"
    _                   -> "Något gick fel. Vänligen försök igen om en stund."

netsTerminalModal :: JSX
netsTerminalModal =
  DOM.div
    { className: "vetrina--payment-wrapper font-duplexsans font-light text-base p-4 border-neutral border-2"
    , children:
      [ DOM.div_
          [ DOM.text "Betalningen öppnas i ett nytt fönster. Följ anvisningarna i det nya fönstret. Du kommer vidare till bekräftelsen när betalningen genomförts. Vid problem ta kontakt med vår kundtjänst på pren@ksfmedia.fi."
          ]
      ]
    }

scaRequired :: Maybe String -> (Boolean -> Effect Unit) -> JSX
scaRequired Nothing _ =
    DOM.div
      { className: "vetrina--payment-wrapper font-duplexsans font-light text-base p-4 border-neutral border-2"
      , children: [ DOM.div_ [ DOM.text "Något gick fel. Vänligen försök igen om en stund." ] ]
      }
scaRequired (Just paymentTerminalUrl) setScaShown =
  let handleClick :: Effect Unit
      handleClick = do
        globalWindow <- Web.HTML.window
        _ <- Window.open paymentTerminalUrl "_blank" "noopener" globalWindow
        setScaShown true
  in DOM.div
    { className: "vetrina--payment-wrapper"
    , children:
      [ DOM.div_
          [ DOM.text "Betalningen kräver ytterligare bekräftelse. Vänligen tryck på knappen nedan för att fortsätta."
          , DOM.div_
              [ DOM.button
                  { className: "vetrina--button"
                  , onClick: handler preventDefault $ const handleClick
                  , children: [ DOM.text "Fortsätt" ]
                  }
              ]
          ]
      ]
    }

-- Render for initial state of Vetrina with no user data
staticRender :: Maybe Paper -> Array Product -> Maybe JSX  -> JSX
staticRender paper products headline = -- headline paper
  vetrinaContainer NewPurchase $ Purchase.NewPurchase.render
  { accountStatus: NewAccount
  , products
  , askAccountAlways: false
  , accessEntitlements: mempty
  , loading: \_ _ -> pure unit
  , startOrder: \_ _ _ _ -> pure unit
  , purchaseError: \_ _ _ _ -> pure unit
  , setRetryPurchase: \_ -> pure unit
  , setUser: \_ -> pure unit
  , productSelection: Nothing
  , onLogin: mempty
  , headline
  , paper
  , paymentMethod: Nothing
  , paymentMethods: []
  , onPaymentMethodChange: \_ -> pure unit
  , onEmailChange: pure unit
  , customRender: Nothing
  , orderSource: PaywallSource
  }
  { emailAddress: Nothing
  , password: Nothing
  , serverErrors: []
  , errorMessage: mempty
  , productSelection: Nothing
  , paymentMethod: Nothing
  , showProductContents: false
  }
  (const $ pure unit)
  mempty
