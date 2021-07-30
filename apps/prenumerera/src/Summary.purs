module Prenumerera.Summary where

import Prelude

import Bottega.Models (PaymentMethod(..))
import Data.Foldable (foldr)
import Data.Maybe (fromMaybe, maybe)
import Data.Nullable (toMaybe)
import KSF.Helpers (formatEur, paperInvoiceCents)
import KSF.User (User)
import KSF.User.Cusno as Cusno
import Prenumerera.Package (Package, PackageOffer)
import Prenumerera.PackageDescription (Description)
import React.Basic (JSX)
import React.Basic.DOM as DOM

render :: User -> Description -> PackageOffer -> PaymentMethod -> JSX
render user description offer method =
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
                , prop "Adress" $
                  maybe ["", "", "", ""]
                  (\address -> [ address.streetAddress
                               , fromMaybe "" $ toMaybe address.zipCode
                               , fromMaybe "" $ toMaybe address.city
                               , address.countryCode
                               ]) $ toMaybe user.address
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

