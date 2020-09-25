module Bottega.Models.FailReason where

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