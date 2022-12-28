module Prenumerera.Summary where

import Prelude

import Bottega.Models (PaymentMethod(..))
import Data.Date (Date)
import Data.Date as Date
import Data.Foldable (foldMap, foldr)
import Data.Maybe (fromMaybe)
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

render :: Date -> User -> Description -> PackageOffer -> PaymentMethod -> JSX
render today user description offer method =
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
            , children:
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
                ]
            }
        , DOM.div
            { className: "summary-order"
            , children:
                [ header "Prenumeration" ] <>
                case method of
                  PaperInvoice ->
                    [ prop "Betalningssätt" [ "Faktura" ]
                    , prop "Pris" [ formatEur offer.totalPrice <> " €"]
                    , prop "Tilläggsavgift" [ formatEur paperInvoiceCents <>
                                              " € faktureringstillägg per pappersfaktura" ]
                    , prop "Totalt" [ formatEur (offer.totalPrice+paperInvoiceCents) <> " €" ]
                    ]
                  CreditCard ->
                    [ prop "Betalningssätt" [ "Kreditkort" ]
                    , prop "Pris" [ formatEur offer.totalPrice ]
                    ]
            }
        ]
    }
  where
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
