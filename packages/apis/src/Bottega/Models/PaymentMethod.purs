module Bottega.Models.PaymentMethod where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String

data PaymentMethod
  = CreditCard
  | PaperInvoice

newtype PaymentMethodId = PaymentMethodId Int
derive instance eqPaymentMethodId :: Eq PaymentMethodId

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
