module Bottega.Models.Order where

import Prelude

import Bottega.Models.FailReason (FailReason(..), parseFailReason)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe (..), maybe)
import Data.Either (note)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.UUID (UUID, parseUUID)

newtype OrderNumber = OrderNumber String

instance decodeJsonOrderNumber :: DecodeJson OrderNumber where
  decodeJson = map OrderNumber <<< decodeJson

newtype Order = Order
  { number     :: OrderNumber
  , user       :: UUID
  , status     :: OrderStatus
  }

derive instance newtypeOrder :: Newtype Order _

instance decodeJsonOrder :: DecodeJson Order where
  decodeJson json = do
    obj <- decodeJson json
    number <- obj .: "number"
    user   <- note (TypeMismatch "Could not parse UUID of user!") <<< parseUUID =<< obj .: "user"
    status <- obj .: "status"
    pure $ Order { number, user, status }

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

instance decodeJsonOrderState :: DecodeJson OrderState where
  decodeJson = decodeJson >>> map (flip parseOrderState $ Nothing)

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
