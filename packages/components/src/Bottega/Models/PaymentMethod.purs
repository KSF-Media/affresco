module Bottega.Models.PaymentMethod where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import KSF.Api (UUID, UserAuth, oauthToken)
import KSF.Api.Package (Package)
import OpenApiClient (Api, callApi)

data PaymentMethod = CreditCard

newtype PaymentMethodId = PaymentMethodId Int

derive instance genericPaymentMethod :: Generic PaymentMethod _
instance showPaymentMethod :: Show PaymentMethod where
  show = genericShow

