module Prenumerera.Summary where

import Prelude

import Bottega.Models (PaymentMethod(..))
import Data.Date (Date)
import Data.Date as Date
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, foldr)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.Time.Duration (Days(..))
import KSF.Helpers (formatEur, paperInvoiceCents)
import KSF.User (User)
import KSF.User.Address (effectiveAddress)
import KSF.User.Cusno as Cusno
import Prenumerera.Package (PackageOffer)
import Prenumerera.Package.Description (Description)
import React.Basic (JSX)
import React.Basic.DOM as DOM

data GiftState = Purchasing PaymentDetails | CodeReady String PaymentDetails | Redeemed

type PaymentDetails =
  { offer :: PackageOffer
  , method :: PaymentMethod
  }

toDetails :: forall f. Applicative f => Maybe String -> Maybe String -> Boolean -> f PaymentDetails -> f (Either GiftState PaymentDetails)
toDetails (Just _) _ _ = const $ pure $ Left Redeemed
toDetails _ Nothing true = map $ Left <<< Purchasing
toDetails _ (Just code) true = map $ Left <<< CodeReady code
toDetails _ _ _ = map Right

render :: Date -> User -> Description -> Either GiftState PaymentDetails -> JSX
render today user description orderDetails =
  DOM.div
    { className: "order-summary"
    , children:
        [ DOM.div
            { className: "summary-package"
            , children:
                [ header "Läspaket"
                , DOM.strong_ [ DOM.text $ description.brandLong <> " " <> description.descriptionShort ]
                , description.descriptionLong
                ]
            }
        , DOM.div
            { className: "summary-customer"
            , children: customerContact
            }
        , DOM.div
            { className: "summary-order"
            , children:
                [ header "Prenumeration" ] <>
                case {details: getPaymentDetails, gift: either Just (const Nothing) orderDetails} of
                  { gift: Just Redeemed, details: _ } ->
                    [ prop "Betalningssätt" [ "Present" ] ]
                  { details: Just {method: PaperInvoice, offer}, gift } ->
                    [ prop "Betalningssätt" [ "Faktura" ]
                    , prop "Pris" [ formatEur offer.totalPrice <> " €"]
                    , prop "Tilläggsavgift" [ formatEur paperInvoiceCents <>
                                              " € faktureringstillägg per pappersfaktura" ]
                    , prop "Totalt" [ formatEur (offer.totalPrice+paperInvoiceCents) <> " €" ]
                    ] <> maybe mempty giftDetails gift
                  { details: Just {method: CreditCard, offer}, gift } ->
                    [ prop "Betalningssätt" [ "Kreditkort" ]
                    , prop "Pris" [ formatEur offer.totalPrice ]
                    ] <> maybe mempty giftDetails gift
                  _ -> mempty
            }
        ]
    }
  where
    getPaymentDetails = case orderDetails of
      Right d -> Just d
      Left (Purchasing d) -> Just d
      Left (CodeReady _ d) -> Just d
      _ -> Nothing
    orderDate = fromMaybe today $ Date.adjust (Days 1.0) today
    header title =
      DOM.div
        { className: "summary-header"
        , children: [ DOM.text title ]
        }
    prop :: String -> Array String -> JSX
    prop title props =
      DOM.p_ $
        [ DOM.span { className: "prop", children: [ DOM.text title ] } ] <>
        foldr (\x xs -> ([ DOM.br {}, DOM.text x ] <> xs)) [] props

    customerContact =
      [ header "Kontaktuppgifter"
      , prop "Namn" [ (fromMaybe "" $ toMaybe user.firstName) <> " " <>
                      (fromMaybe "" $ toMaybe user.lastName) ]
      , prop "E-post" [ user.email ]
      , foldMap
        (\address -> prop "Adress"
                     [ address.streetAddress
                     , address.zipcode
                     , fromMaybe "" address.city
                     , address.countryCode
                     ]) $ effectiveAddress user orderDate
      , prop "Kundnummer" [ Cusno.toString user.cusno ]
      ] <> giftAddress orderDetails

    giftAddress (Left (Purchasing _)) =
      [ DOM.text "Lahjan saaja syöttää osoitteen lunastushetkellä"
      ]
    giftAddress (Left (CodeReady _ _)) =
      [ DOM.text "Lahjan saaja syöttää osoitteen lunastushetkellä"
      ]
    giftAddress _ = mempty

    giftDetails :: GiftState -> Array JSX
    giftDetails (Purchasing _) =
      [ prop "Presentkod" [ "Skall genereras" ] ]
    giftDetails (CodeReady code _) =
      [ DOM.p_ $
          [ DOM.span { className: "prop", children: [ DOM.text "Presentkod" ] }
          , DOM.br {}
          , DOM.a { href: "http://localhost:8000/present/" <> code
                  , children: [ DOM.text code ]
                  }
          , DOM.br {}
          , DOM.text "Saat lahjakoodin myös sähköpostilla"
          ]
      ]
    giftDetails _ = mempty
