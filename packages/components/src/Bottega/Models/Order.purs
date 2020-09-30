module Bottega.Models.Order where

import Prelude (($))

import Bottega.Models.FailReason (FailReason(..), parseFailReason)
import Data.Maybe (Maybe, maybe)
import KSF.Api (UUID)

newtype OrderNumber = OrderNumber String

type Order =
  { number     :: OrderNumber
  , user       :: UUID
  , status     :: OrderStatus
  }

type OrderStatus =
  { state      :: OrderState
  , time       :: String
  }

data OrderState
  = OrderCreated
  | OrderStarted
  | OrderCompleted
  | OrderFailed FailReason
  | OrderCanceled
  | OrderUnknownState

parseOrderState :: String -> Maybe String -> OrderState
parseOrderState state maybeFailReason =
    case state of
      "created"   -> OrderCreated
      "started"   -> OrderStarted
      "completed" -> OrderCompleted
      "failed"    -> OrderFailed $ maybe UnknownReason parseFailReason maybeFailReason
      "canceled"  -> OrderCanceled
      _           -> OrderUnknownState

type NewOrder =
  { packageId      :: String
  , period         :: Int
  , payAmountCents :: Int
  , campaignNo     :: Maybe Int
  }