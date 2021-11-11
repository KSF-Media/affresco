module Bottega.Models.Order where

import Prelude

import Bottega.Models.FailReason (FailReason(..), parseFailReason)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.UUID (UUID)

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

derive instance genericOrderNumber :: Generic OrderNumber _
instance showOrderNumber :: Show OrderNumber where
  show = genericShow

derive instance genericOrderState :: Generic OrderState _
instance showOrderState :: Show OrderState where
  show = genericShow

derive instance eqOrderState :: Eq OrderState

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
  , orderSource    :: Maybe OrderSource
  }

data OrderSource = CampaignPagesSource
                 | PrenumereraSource
                 | PaywallSource
                 | UnknownSource

toOrderSource :: String -> OrderSource
toOrderSource orderSource =
  case String.toLower orderSource of
    "campaignpagessource" -> CampaignPagesSource
    "prenumererasource"   -> PrenumereraSource
    "paywallsource"       -> PaywallSource
    _                     -> UnknownSource

fromOrderSource :: OrderSource -> String
fromOrderSource orderSource =
  case orderSource of
    CampaignPagesSource -> "CampaignpagesSource"
    PrenumereraSource   -> "PrenumereraSource"
    PaywallSource       -> "PaywallSource"
    _                   -> "UnknownSource"
