module KSF.PaymentMethod where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import KSF.User (PaymentMethod(..))

paymentMethod :: (Maybe PaymentMethod -> Effect Unit) -> JSX
paymentMethod onChange =
  DOM.div
    { className: "payment-method--payment-options"
    , children: [ paymentMethodOption onChange CreditCardPayment ]
    }

paymentMethodOption :: (Maybe PaymentMethod -> Effect Unit) -> PaymentMethod -> JSX
paymentMethodOption onChange method =
  let (Tuple methodString methodDescription) = case method of
        CreditCardPayment -> Tuple "credit-card" "Kreditkort"
        PaperInvoicePayment -> Tuple "paper-invoice" "Faktura"
  in DOM.div_
       [ DOM.input
         { className: ""
         , type: "radio"
         , name: "payment-method"
         , value: methodString
         , id: methodString
         , onChange: handler targetValue ((_ >>= stringToPaymentMethod) >>> onChange)
         }
       , DOM.label
           { className: ""
           , htmlFor: methodString
           , children: [ DOM.text methodDescription ]
           }
       ]

paymentMethodString :: PaymentMethod -> String
paymentMethodString CreditCardPayment = "credit-card"
paymentMethodString PaperInvoicePayment = "paper-invoice"

stringToPaymentMethod :: String -> Maybe PaymentMethod
stringToPaymentMethod p = case p of
  "credit-card" -> Just CreditCardPayment
  "paper-invoice" -> Just PaperInvoicePayment
  _ -> Nothing
