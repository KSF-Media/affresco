module Bottega.Models.PaymentMethod where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String as String

data PaymentMethod
  = CreditCard
  | PaperInvoice

newtype PaymentMethodId = PaymentMethodId Int

instance eqPaymentMethodId :: Eq PaymentMethodId where
  eq (PaymentMethodId a) (PaymentMethodId b) = a == b

toPaymentMethod :: String -> Maybe PaymentMethod
toPaymentMethod paymentMethod =
  case String.toLower paymentMethod of
    "creditcard"   -> Just CreditCard
    "paperinvoice" -> Just PaperInvoice
    _              -> Nothing

derive instance genericPaymentMethod :: Generic PaymentMethod _
instance showPaymentMethod :: Show PaymentMethod where
  show = genericShow

instance eqPaymentMethod :: Eq PaymentMethod where
  eq CreditCard CreditCard = true
  eq PaperInvoice PaperInvoice = true
  eq _ _ = false
