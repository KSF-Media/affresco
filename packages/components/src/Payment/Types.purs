module KSF.Payment.Types where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.List as List
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as FormatterD
import Data.Formatter.Number as FormatterN
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import KSF.Api (UUID)
import KSF.User (SubscriptionPayments, PaymentState(..), PaymentType(..))
import KSF.User as User
import React.Basic.Hooks (type (/\), Render, UseEffect)

type Props =
  { usePayments :: Render Unit (UseEffect (Boolean /\ Maybe String) Unit) Unit
  , subscriptionPayments :: Maybe (Array SubscriptionPayments)
  }

getPayments :: Maybe UUID -> (Array SubscriptionPayments -> Effect Unit) -> (Aff Boolean -> Effect Unit) -> Effect Unit
getPayments Nothing _ _ = pure unit
getPayments (Just uuid) setPayments withSpinner = do
  withSpinner do
    p <- User.getPayments uuid
    case p of
      Right payments -> do
        liftEffect $ setPayments payments
        pure true
      _ -> pure false

{-
getPayments uuid setPayments withSpinner =
 = case state.payments /\ state.activeUser of
                            Nothing /\ Just user -> withSpinner do
                              p <- User.getPayments user.uuid
                              case p of
                                Right p' -> do
                                  liftEffect setState $ Types.setPayments $ Just p'
                                  pure true
                                _ -> pure false
                            _ -> pure unit)
-}

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

percentFormatter :: FormatterN.Formatter
percentFormatter = FormatterN.Formatter
  { comma: false
  , before: 1
  , after: 0
  , abbreviations: false
  , sign: false
  }
