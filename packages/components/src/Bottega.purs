module Bottega where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import KSF.Api (UUID, UserAuth, oauthToken)
import KSF.Api.Package (Package)
import Simple.JSON (class ReadForeign, read, readImpl)

import OpenApiClient (Api, callApi)

foreign import ordersApi :: Api
foreign import packagesApi :: Api

createOrder :: UserAuth -> NewOrder -> Aff Order
createOrder { userId, authToken } newOrder =
  readOrder =<< callApi ordersApi "orderPost" [ unsafeToForeign newOrder ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getOrder :: UserAuth -> OrderNumber -> Aff Order
getOrder { userId, authToken } orderNumber = do
  readOrder =<< callApi ordersApi "orderOrderNumberGet" [ unsafeToForeign orderNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

readOrder :: { number :: OrderNumber, user :: UUID, status :: { state :: Foreign, time :: String, failReason :: Nullable String } } -> Aff Order
readOrder orderObj = do
  orderStatus <- case read orderObj.status.state of
    Right status -> pure status
    Left err     -> pure UnknownState
  failureReason <- case orderStatus of
    UnknownState -> pure Nothing
    _ -> pure $ do
      reasonString <- toMaybe orderObj.status.failReason
      reason       <- parseFailReason reasonString
      pure reason
  pure $ orderObj { status { state = orderStatus, failReason = failureReason } }

payOrder :: UserAuth -> OrderNumber -> PaymentMethod -> Aff PaymentTerminalUrl
payOrder { userId, authToken } orderNumber paymentMethod =
  callApi ordersApi "orderOrderNumberPayPost" [ unsafeToForeign orderNumber, unsafeToForeign { paymentOption: show paymentMethod } ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

getPackages :: Aff (Array Package)
getPackages = callApi packagesApi "packageGet" [] {}

newtype OrderNumber = OrderNumber String

type Order =
  { number     :: OrderNumber
  , user       :: UUID
  , status     :: OrderStatus
  }

type OrderStatus =
  { state      :: OrderStatusState
  , time       :: String
  , failReason :: Maybe OrderStatusFailReason
  }

data OrderStatusState
  = OrderCreated
  | OrderStarted
  | OrderCompleted
  | OrderFailed
  | OrderCanceled
  | UnknownState

derive instance genericOrderStatusState :: Generic OrderStatusState _
instance readOrderStatusState :: ReadForeign OrderStatusState where
  readImpl foreignOrderStatusState = do
    orderStatusStateString :: String <- readImpl foreignOrderStatusState
    case orderStatusStateString of
      "created"   -> pure OrderCreated
      "started"   -> pure OrderStarted
      "completed" -> pure OrderCompleted
      "failed"    -> pure OrderFailed
      "canceled"  -> pure OrderCanceled
      _           -> pure UnknownState

data OrderStatusFailReason
  = NetsInternalError
  | NetsIssuerError
  | NetsCanceled
  | SubscriptionExistsError
  | SubscriptionError
  | OrderNotFound
  | UnknownReason

parseFailReason :: String -> Maybe OrderStatusFailReason
parseFailReason reason =
  case reason of
    "NetsInternalError"       -> pure NetsInternalError
    "NetsIssuerError"         -> pure NetsIssuerError
    "NetsCanceled"            -> pure NetsCanceled
    "SubscriptionExistsError" -> pure SubscriptionExistsError
    "SubscriptionError"       -> pure SubscriptionError
    "OrderNotFound"           -> pure OrderNotFound
    _                         -> pure UnknownReason

type NewOrder =
  { packageId      :: String
  , period         :: Int
  , payAmountCents :: Int
  }

data PaymentMethod = CreditCard

derive instance genericProvider :: Generic PaymentMethod _
instance showProvider :: Show PaymentMethod where
  show = genericShow

type PaymentTerminalUrl = { paymentTerminalUrl :: String }
