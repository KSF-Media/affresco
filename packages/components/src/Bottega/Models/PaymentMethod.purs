module Bottega.Models.PaymentMethod where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data PaymentMethod
  = CreditCard
  | PaperInvoice

newtype PaymentMethodId = PaymentMethodId Int

derive instance genericPaymentMethod :: Generic PaymentMethod _
instance showPaymentMethod :: Show PaymentMethod where
  show = genericShow
