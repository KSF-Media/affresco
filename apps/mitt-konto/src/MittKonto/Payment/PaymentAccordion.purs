module MittKonto.Payment.PaymentAccordion where

import Prelude

import Data.Array (length, reverse, zip)
import Data.Enum (enumFromTo)
import Data.Formatter.Number as FormatterN
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign (unsafeToForeign)
import KSF.Helpers (formatDateDots)
import KSF.User (SubscriptionPayments)
import MittKonto.Payment.Types (Props, stateString, typeString, formatEuro, percentFormatter)
import React.Basic (JSX)
import React.Basic.Hooks (Component, component, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import Routing.PushState (PushStateInterface)

paymentAccordion :: Component Props
paymentAccordion = do
  component "PaymentAccordion" \ { usePayments, subscriptionPayments, router } -> React.do
    usePayments
    focus /\ setFocus <- useState' Nothing
    case subscriptionPayments of
      Nothing -> pure $ DOM.text "Något gick fel!"
      Just payments -> pure $ render router payments focus setFocus

render :: PushStateInterface -> Array SubscriptionPayments -> Maybe Int -> (Maybe Int -> Effect Unit) -> JSX
render router payments focus setFocus =
  DOM.div
    { className: "payment-accordion--payments"
    , children: if length payments <= 0 then renderEmpty
                else map renderSubscription $ zip (enumFromTo 0 (length payments-1)) payments
    }
  where
    renderEmpty = [ DOM.text "Du har inga fakturor." ]
    renderSubscription (Tuple i subscription) =
      DOM.div
        { className: "payment-accordion-subscription mitt-konto--component-block-content"
        , children:
            [ DOM.div
                { className: "payment-accordion--header"
                    <> (if focus == Just i then " focus" else " nofocus")
                , children:
                    [ DOM.span_ [ DOM.text subscription.name ]
                    , DOM.span_ [ DOM.text $ (formatDateDots subscription.startDate)
                                    <> " - " <> (formatDateDots subscription.lastDate) ]
                    , DOM.div_ [ ]
                    ]
                , onClick: handler_ $ setFocus $ Just i
                }
            ]  <>
            (if focus == Just i
               then [ DOM.div
                        { className: "payment-accordion--details-container"
                        , children:
                            [ DOM.table
                                { className: "payment-accordion--details"
                                , children: [ DOM.thead_ [ headerRow ]
                                            , DOM.tbody_ $ renderPayment <$> reverse subscription.payments
                                            ]
                                }
                            ]
                        }
                    ]
               else []
            )
        }
    headerRow =
      DOM.tr_ [ DOM.th_ [ DOM.text "Utskrivningsdatum" ]
              , DOM.th_ [ DOM.text "Typ" ]
              , DOM.th_ [ DOM.text "Status" ]
              , DOM.th_ [ DOM.text "Moms %" ]
              , DOM.th_ [ DOM.text "Moms" ]
              , DOM.th_ [ DOM.text "Belopp" ]
              , DOM.th_ [ DOM.text "Faktureringstillägg" ]
              , DOM.th_ [ DOM.text "Obetalt" ]
              , DOM.th_ [ DOM.text "Totalt" ]
              ]
    renderPayment payment =
      DOM.tr
        { className: "payment-accordion--item"
        , children:
            map (_ $ \x -> DOM.a
                             { onClick: (handler preventDefault $ const $
                                         router.pushState (unsafeToForeign {}) ("/fakturor/" <> show payment.invno))
                             , children: x
                             })
            [ \f -> DOM.td_ [ f [ DOM.text $ formatDateDots payment.date ] ]
            , \f -> DOM.td_ [ f [ DOM.text $ typeString payment.type ] ]
            , \f -> DOM.td_ [ f [ DOM.text $ stateString payment.state ] ]
            , \f -> percent f $ if payment.amount > 0.01
                                  then 100.0 * payment.vat / payment.amount
                                  else 0.0
            , \f -> euro f payment.vat
            , \f -> euro f payment.amount
            , \f -> euro f $ payment.expenses + payment.interest
            , \f -> euro f payment.openAmount
            , \f -> euro f $ payment.expenses + payment.interest + payment.vat + payment.amount
            ]
        }
    euro :: (Array JSX -> JSX) -> Number -> JSX
    euro f x = DOM.td { className: "euro"
                      , children: [ f [ DOM.text $ formatEuro x ] ]
                      }
    percent :: (Array JSX -> JSX) -> Number -> JSX
    percent f x = DOM.td { className: "euro"
                         , children: [ f [ DOM.text $ FormatterN.format percentFormatter x ] ]
                         }
