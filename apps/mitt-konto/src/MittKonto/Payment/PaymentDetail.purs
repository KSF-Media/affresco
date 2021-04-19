module MittKonto.Payment.PaymentDetail where

import Prelude

import Data.Array (last)
import Data.Foldable (find)
import Data.Formatter.Number as FormatterN
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.String as String
import KSF.Helpers (formatDateDots)
import KSF.User (Payment)
import MittKonto.Payment.Types (Props, percentFormatter, formatEuro)
import React.Basic (JSX)
import React.Basic.Hooks (type (/\), Component, component, unsafeRenderEffect, (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (pathname)

paymentDetail :: Component Props
paymentDetail = do
  component "PaymentDetail" \ { usePayments, subscriptionPayments } -> React.do
    usePayments
    let findPayment :: Int -> Maybe (String /\ Payment)
        findPayment invno =
          join $ find isJust $
          map (\x -> (x.name /\ _) <$> (find (\y -> y.invno == invno) x.payments))
          (fromMaybe mempty subscriptionPayments)
    -- TODO useParams
    maybePayment <- (findPayment <=< fromString <=< last) <<< String.split (String.Pattern "/")
                      <$> unsafeRenderEffect (window >>= location >>= pathname)
    case isJust subscriptionPayments /\ maybePayment of
      false /\ _ -> do
        pure $ DOM.div_ [ DOM.text "Något gick fel!" ]
      _ /\ Nothing -> do
        pure $ DOM.div_ [ DOM.text "Fakturan kunde inte hittas." ]
      _ /\ Just (productName /\ payment) -> React.do
        pure $ render productName payment

render :: String -> Payment -> JSX
render name payment =
  DOM.div
    { className: "payment-detail--table-container mitt-konto--component-block-content"
    , children:
        [ DOM.table
            { className: "payment-detail--table"
            , children:
                [ DOM.tbody_ $ map renderRow
                    [ "Betaldatum" /\ formatDateDots payment.date
                    , "Mottagare" /\ "KSF Media Ab"
                    , "Adress" /\ "Mannerheimvägen 18, 00100 HELSINGFORS"
                    , "Momsnummer" /\ "FI21372401"
                    , "Produkt" /\ name
                    , ("Moms" <> taxPercent) /\ formatEuro payment.vat
                    , "Totalt" /\ formatEuro
                        (payment.expenses + payment.interest + payment.vat + payment.amount)
                    , "Referensnummer" /\ payment.reference
                    ]
                ]
            }
        ]
    }
  where
    taxPercent = if payment.amount > 0.01
                 then " (" <> (FormatterN.format percentFormatter
                               (100.0 * payment.vat / payment.amount)) <> "%)"
                 else ""
    renderRow :: (String /\ String) -> JSX
    renderRow (header /\ msg) =
      DOM.tr_ [ DOM.th_ [ DOM.text header ]
              , DOM.td_ [ DOM.text msg ]
              ]
