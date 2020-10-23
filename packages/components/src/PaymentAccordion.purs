module KSF.PaymentAccordion where

import Prelude

import Data.Array (length, reverse, zip)
import Data.Enum (enumFromTo)
import Data.List as List
import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as FormatterD
import Data.Formatter.Number as FormatterN
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.User (SubscriptionPayments, PaymentState(..), PaymentType(..))
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Self = React.Self Props State

type Props =
  { paymentsLoad :: Aff (Array SubscriptionPayments)
  }

type State =
  { focus :: Maybe Int
  , payments :: Array SubscriptionPayments
  }

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.createComponent "PaymentList"

payments :: Props -> JSX
payments = make component
  { initialState: { focus: Nothing
                  , payments: []
                  }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self = do
  Aff.launchAff_ $ do
    p <- self.props.paymentsLoad
    liftEffect $ self.setState \s -> s { payments = p }

render :: Self -> JSX
render self =
  DOM.div
    { className: "payment-accordion--payments"
    , children: if length self.state.payments <= 0 then renderEmpty
                else map renderSubscription $ zip (enumFromTo 0 (length self.state.payments-1)) self.state.payments
    }
  where
    renderEmpty = [ DOM.text "Du har inga fakturor." ]
    renderSubscription (Tuple i subscription) =
      DOM.div
        { className: "payment-accordion-subscription mitt-konto--component-block-content"
        , children:
            [ DOM.div
                { className: "payment-accordion--header"
                    <> (if self.state.focus == Just i then " focus" else " nofocus")
                , children:
                    [ DOM.span_ [ DOM.text subscription.name ]
                    , DOM.span_ [ DOM.text $ (formatDate subscription.startDate)
                                    <> " - " <> (formatDate subscription.lastDate) ]
                    , DOM.div_ [ ]
                    ]
                , onClick: handler_ $ self.setState _ { focus = Just i }
                }
            ]  <>
            (if self.state.focus == Just i
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
      DOM.tr_ [ DOM.th_ [ DOM.text "Förfallodag" ]
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
            [ DOM.td_ [ DOM.text $ formatDate payment.dueDate ]
            , DOM.td_ [ DOM.text $ typeString payment.type ]
            , DOM.td_ [ DOM.text $ stateString payment.state ]
            , percent $ if payment.amount > 0.01
                          then 100.0 * payment.vat / payment.amount
                          else 0.0
            , euro payment.vat
            , euro payment.amount
            , euro $ payment.expenses + payment.interest
            , euro payment.openAmount
            , euro $ payment.expenses + payment.interest + payment.vat + payment.amount
            ]
        }
    euro x = DOM.td { className: "euro"
                    , children: [ DOM.text $ formatEuro x ]
                    }
    percent x = DOM.td { className: "euro"
                       , children: [ DOM.text $ FormatterN.format percentFormatter x ]
                       }
    percentFormatter = FormatterN.Formatter
      { comma: false
      , before: 1
      , after: 0
      , abbreviations: false
      , sign: false
      }

formatDate :: Date -> String
formatDate = FormatterD.format formatter <<< \x -> DateTime x bottom
  where
    dot = Placeholder "."
    formatter = List.fromFoldable
      [ DayOfMonthTwoDigits
      , dot
      , MonthTwoDigits
      , dot
      , YearFull
      ]

formatEuro :: Number -> String
formatEuro = FormatterN.format formatter
  where
    formatter = FormatterN.Formatter
      { comma: false
      , before: 1
      , after: 2
      , abbreviations: false
      , sign: false
      }

stateString :: PaymentState -> String
stateString x = case x of
  PaymentOpen   -> "Öppen"
  PartiallyPaid -> "Delvis betald"
  Paid          -> "Betald"
  Reminded      -> "Påmind"
  Foreclosure   -> "Avslutad pga obetald faktura"
  Reimbursed    -> "Krediterad"
  CreditLoss    -> "Kreditförlust"

typeString :: PaymentType -> String
typeString x = case x of
  NormalState   -> "Normal"
  DirectDebit   -> "Direktdebitering"
  Reminder1     -> "1. påminnelse"
  Reminder2     -> "2. påminnelse"
  Nonpayment    -> "Avslutad utan betalning, fakturerad"
  Reimbursement -> "Kreditfaktura"
