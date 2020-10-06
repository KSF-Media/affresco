module KSF.PaymentAccordion.Component where

import Prelude

import Data.Array (length, zip)
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
import KSF.User (SubscriptionPayments, PaymentState(..), PaymentType(..))
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Self = React.Self Props State

type Props =
  { payments :: Array SubscriptionPayments
  }

type State =
  { focus :: Maybe Int
  }

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.createComponent "PaymentList"

payments :: Props -> JSX
payments = make component
  { initialState: { focus: Nothing }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self = do
  pure unit

render :: Self -> JSX
render self =
  DOM.div
    { className: "mitt-konto--payments"
    , children: map renderSubscription $ zip (enumFromTo 0 (length self.props.payments-1)) self.props.payments
    }
  where
    renderSubscription (Tuple i subscription) =
      DOM.div
        { className: "mitt-konto--payment-subscription mitt-konto--component-block-content"
        , children:
            [ DOM.div
                { className: "mitt-konto--payment-header"
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
               then [ DOM.table
                        { className: "mitt-konto--payment-details"
                        , children: [ DOM.thead_ [ headerRow ]
                                    , DOM.tbody_ $ renderPayment <$> subscription.payments
                                    ]
                        }
                    ]
               else []
            )
        }
    headerRow =
      DOM.tr_ [ DOM.th_ [ DOM.text "date" ]
              , DOM.th_ [ DOM.text "dueDate" ]
              , DOM.th_ [ DOM.text "type" ]
              , DOM.th_ [ DOM.text "state" ]
              , DOM.th_ [ DOM.text "expenses" ]
              , DOM.th_ [ DOM.text "interest" ]
              , DOM.th_ [ DOM.text "vat" ]
              , DOM.th_ [ DOM.text "amount" ]
              , DOM.th_ [ DOM.text "openAmount" ]
              , DOM.th_ [ DOM.text "discount" ]
              ]
    renderPayment payment =
      DOM.tr
        { className: "mitt-konto--payment-item"
        , children:
            [ DOM.td_ [ DOM.text $ formatDate payment.date ]
            , DOM.td_ [ DOM.text $ formatDate payment.dueDate ]
            , DOM.td_ [ DOM.text $ typeString payment.type ]
            , DOM.td_ [ DOM.text $ stateString payment.state ]
            , euro payment.expenses
            , euro payment.interest
            , euro payment.vat
            , euro payment.amount
            , euro payment.openAmount
            , euro payment.discount
            ]
        }
    euro x = DOM.td { className: "euro"
                    , children: [ DOM.text $ formatEuro x ]
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
  PaymentOpen   -> "open"
  PartiallyPaid -> "part"
  Paid          -> "paid"
  Reminded      -> "reminded"
  Foreclosure   -> "forec"
  Reimbursed    -> "reimb"
  CreditLoss    -> "loss"

typeString :: PaymentType -> String
typeString x = case x of
  NormalState   -> "normal"
  DirectDebit   -> "suora"
  Reminder1     -> "muistutus 1"
  Reminder2     -> "muistutus 2"
  Nonpayment    -> "tappio"
  Reimbursement -> "hyvitys"
