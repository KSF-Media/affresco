module Bottega.Models.FailReason where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
