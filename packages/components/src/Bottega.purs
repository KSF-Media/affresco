module Bottega where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
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

readOrder :: { number :: OrderNumber, user :: UUID, status :: { state :: String, time :: String, failReason :: Nullable String } } -> Aff Order
readOrder orderObj = do
  let state = parseStatus orderObj.status.state (toMaybe orderObj.status.failReason)
  pure $ { number: orderObj.number, user: orderObj.user, status: { state, time: orderObj.status.time }}

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
  }

data OrderStatusState
  = OrderCreated
  | OrderStarted
  | OrderCompleted
  | OrderFailed OrderStatusFailReason
  | OrderCanceled
  | UnknownState

parseStatus :: String -> Maybe String -> OrderStatusState
parseStatus state maybeFailReason =
    case state of
      "created"   -> OrderCreated
      "started"   -> OrderStarted
      "completed" -> OrderCompleted
      "failed"    -> OrderFailed $ maybe UnknownReason parseFailReason maybeFailReason
      "canceled"  -> OrderCanceled
      _           -> UnknownState


data OrderStatusFailReason
  = NetsInternalError
  | NetsIssuerError
  | NetsCanceled
  | SubscriptionExistsError
  | SubscriptionError
  | OrderNotFound
  | UnknownReason

derive instance genericOrderStatusFailReason :: Generic OrderStatusFailReason _
instance showOrderStatusFailReason :: Show OrderStatusFailReason where
  show = genericShow

parseFailReason :: String -> OrderStatusFailReason
parseFailReason reason =
  case reason of
    "NetsInternalError"       -> NetsInternalError
    "NetsIssuerError"         -> NetsIssuerError
    "NetsCanceled"            -> NetsCanceled
    "SubscriptionExistsError" -> SubscriptionExistsError
    "SubscriptionError"       -> SubscriptionError
    "OrderNotFound"           -> OrderNotFound
    _                         -> UnknownReason

type NewOrder =
  { packageId      :: String
  , period         :: Int
  , payAmountCents :: Int
  }

data PaymentMethod = CreditCard

derive instance genericPaymentMethod :: Generic PaymentMethod _
instance showPaymentMethod :: Show PaymentMethod where
  show = genericShow

type PaymentTerminalUrl = { paymentTerminalUrl :: String }
