module KSF.PaymentMethod where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

data PaymentMethod = CreditCardPayment

paymentMethod :: (Maybe PaymentMethod -> Effect Unit) -> JSX
paymentMethod onChange =
  DOM.div
    { className: "payment-method--payment-options"
    , children: [ paymentMethodOption CreditCardPayment onChange ]
    }

paymentMethodOption :: PaymentMethod -> (Maybe PaymentMethod -> Effect Unit) -> JSX
paymentMethodOption method onChange =
  let (Tuple methodString methodDescription) = case method of
        CreditCardPayment -> Tuple "credit-card" "Kreditkort"
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

stringToPaymentMethod :: String -> Maybe PaymentMethod
stringToPaymentMethod p = case p of
  "credit-card" -> Just CreditCardPayment
  _ -> Nothing
