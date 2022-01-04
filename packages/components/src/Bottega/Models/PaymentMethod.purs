module Bottega.Models.PaymentMethod where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String

data PaymentMethod
  = CreditCardPayment
  | PaperInvoicePayment

newtype PaymentMethodId = PaymentMethodId Int
derive instance eqPaymentMethodId :: Eq PaymentMethodId

instance decodeJsonPaymenMethodId :: DecodeJson PaymentMethodId where
  decodeJson = map PaymentMethodId <<< decodeJson

toPaymentMethod :: String -> Maybe PaymentMethod
toPaymentMethod paymentMethod =
  case String.toLower paymentMethod of
    "creditcard"   -> Just CreditCardPayment
    "paperinvoice" -> Just PaperInvoicePayment
    _              -> Nothing

derive instance genericPaymentMethod :: Generic PaymentMethod _
instance showPaymentMethod :: Show PaymentMethod where
  show = genericShow

instance eqPaymentMethod :: Eq PaymentMethod where
  eq CreditCardPayment CreditCardPayment = true
  eq PaperInvoicePayment PaperInvoicePayment = true
  eq _ _ = false
