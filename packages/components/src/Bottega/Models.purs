module Bottega.Models where

import Prelude

import Bottega.Models.PaymentMethod as PaymentMethod
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import KSF.Api (UUID, UserAuth, oauthToken)
import KSF.Api.Package (Package)
import OpenApiClient (Api, callApi)

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
  | OrderFailed FailReason
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


data FailReason
  = NetsInternalError
  | NetsIssuerError
  | NetsCanceled
  | SubscriptionExistsError
  | SubscriptionError
  | OrderNotFound
  | UnknownReason

derive instance genericFailReason :: Generic FailReason _
instance showFailReason :: Show FailReason where
  show = genericShow

parseFailReason :: String -> FailReason
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
  , campaignNo     :: Maybe Int
  }

newtype CreditCardId = CreditCardId Int

data CreditCard = CreditCard
  { id              :: CreditCardId
  , user            :: UUID
  , paymentMethodId :: Maybe PaymentMethod.PaymentMethodId
  , panHash         :: Maybe String
  , maskedPan       :: Maybe String
  , expiryDate      :: Maybe String
  }

data CreditCardRegister = CreditCardRegister
  { number      :: OrderNumber
  , user        :: UUID
  , terminalUrl :: Maybe PaymentTerminalUrl
  , status      :: CreditCardRegisterStatus
  }

data CreditCardRegisterStatus = CreditCardRegisterStatus
  { state      :: CreditCardRegisterStatusState
  , time       :: String
  }

data CreditCardRegisterStatusState
  = CreditCardRegisterCreated
  | CreditCardRegisterStarted
  | CreditCardRegisterCompleted
  | CreditCardRegisterFailed FailReason
  | CreditCardRegisterCanceled

type PaymentTerminalUrl = { paymentTerminalUrl :: String }
