module Prenumerera.SelectPeriod where

import Prelude

import Bottega.Models (PaymentMethod(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Effect (Effect)
import KSF.Helpers (formatEur, paperInvoiceCents)
import KSF.InputField.Checkbox as Checkbox
import KSF.Paper as Paper
import Prenumerera.Package (Package, PackageOffer)
import Prenumerera.PackageDescription (Description)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue, preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { package :: Package
  , description :: Description
  , next :: PackageOffer -> PaymentMethod -> Effect Unit
  , cancel :: Effect Unit
  }

type State =
  { offer :: PackageOffer
  , setOffer :: PackageOffer -> Effect Unit
  }

component :: Component Props
component = do
  React.component "SelectPeriod" $ \ { package, description, next } -> React.do
    offer /\ setOffer <- useState' $ head package.offers
    remind /\ setRemind <- useState' false
    paymentMethod /\ setPaymentMethod <- useState' CreditCard
    acceptTerms /\ setAcceptTerms <- useState' false
    let remindElement = guard remind renderRemind
        paymentOfferElement = renderPaymentOffer package.offers offer setOffer paymentMethod setPaymentMethod
        acceptElement = renderAccept acceptTerms setAcceptTerms
    pure $ render description remindElement paymentOfferElement acceptElement
      offer paymentMethod acceptTerms $
      if acceptTerms then next offer paymentMethod else setRemind true

renderRemind :: JSX
renderRemind =
  DOM.div
    { className: "alert alert-warning"
    , children:
        [ DOM.p_
            [ DOM.strong_ [ DOM.text "Hoppsan!" ]
            , DOM.br {}
            , DOM.text "Vänligen godkänn KSF Medias bruksvillkor för att fortsätta."
            ]
        ]
    }

renderPaymentOffer :: NonEmptyArray PackageOffer -> PackageOffer -> (PackageOffer -> Effect Unit) -> PaymentMethod -> (PaymentMethod -> Effect Unit) -> JSX
renderPaymentOffer offers offer setOffer paymentMethod setPaymentMethod =
  DOM.div
    { className: "payment"
    , children:
        [ DOM.label_
            [ DOM.span_ [ DOM.text "Betalningssätt" ]
            , DOM.select
                { id: "payment_option"
                , name: "payment_option"
                , required: true
                , onChange: handler targetValue $ setPaymentMethod <<< decodeMethod
                , children: map renderPaymentOptionOption [ CreditCard, PaperInvoice ]
                }
            ]
        , DOM.label_
            [ DOM.span_ [ DOM.text "Faktureringsperiod" ]
            , DOM.select
                { id: "months"
                , name: "months"
                , onChange: handler targetValue
                    (\newMonths -> 
                        setOffer $ fromMaybe (head offers) $
                        find (\o -> newMonths == Just (show o.months)) offers)
                , children: NonEmpty.toArray $ map renderOfferOption offers
                }
            ]
        , guard (paymentMethod == PaperInvoice) $
            DOM.div
              { className: "alert alert-info"
              , children:
                  [ DOM.p_
                      [ DOM.strong_ [ DOM.text "Obs!" ]
                      , DOM.br {}
                      , DOM.span_ [ DOM.text " På pappersfakturor som levereras per post uppbär vi en tilläggsavgift på 5,00 euro per faktura (inkl. Moms). Du kan i din nätbank byta fakturan till en e-faktura som inte har ett faktureringstillägg. " ]
                      ]
                  ]
              }
        ]
    }
  where
    renderPaymentOptionOption option =
      DOM.option
        { value: if option == CreditCard then "c" else "i"
        , selected: option == paymentMethod
        , children: [ DOM.text $ case option of
                         CreditCard -> "Kreditkort"
                         PaperInvoice -> "Faktura"
                    ]
        }
    decodeMethod (Just "i") = PaperInvoice
    decodeMethod _ = CreditCard
    renderOfferOption { months } =
      DOM.option
        { value: show months
        , selected: months == offer.months
        , children: [ DOM.text $ show months <> " " <>
                      if months == 1 then "månad" else "månader"
                    ]
        }

renderAccept :: Boolean -> (Boolean -> Effect Unit) -> JSX
renderAccept accept setAccept =
  Checkbox.inputCheckbox
    { type_: Checkbox.Checkbox
    , name: "toggle-accept-terms"
    , checked: accept
    , onChange: setAccept
    , labelJSX: Just $
        React.fragment
          [ DOM.text "Jag har läst och godkänner KSF Medias "
          , DOM.a
              { href: "https://www.hbl.fi/bruksvillkor/"
              , target: "_blank"
              , children: [ DOM.text "bruksvillkor" ]
              }
          ]
    , checkboxFirst: true
    }

render :: Description -> JSX -> JSX -> JSX -> PackageOffer -> PaymentMethod -> Boolean -> Effect Unit -> JSX
render description remindElement paymentOfferElement acceptElement offer paymentMethod acceptTerms submit =
  DOM.div
    { className: "container row ksf-identify"
    , children:
        [ DOM.div
            { className: "ksf-auth-wrapper"
            , children:
                [ DOM.div
                    { id: "ksf-accept-terms-form"
                    , className: "ksf-auth-section janrain-api-container"
                    , children:
                        [ DOM.div
                            { className: "ksf-auth-header"
                            , children:
                                [ DOM.h2_ [ DOM.text "Villkor och betalningssätt" ]
                                , DOM.p_
                                    [ DOM.text "Välj betalningssätt och faktureringsperiod för din produkt"
                                    , DOM.strong_
                                        [ DOM.text $ "\"" <> (Paper.toString description.brand) <> " " <>
                                          description.descriptionShort <> "\"" ]
                                    ]
                                , DOM.p_
                                    [ DOM.text "Prenumerationstyp"
                                    , DOM.strong_ [ DOM.text "Fortlöpande" ]
                                    ]
                                ]
                            }
                        , DOM.form
                            { id: "accept-terms-form"
                            , className: "janrain-api-form"
                            , onSubmit: handler preventDefault $ const submit
                            , children:
                                [ remindElement
                                , paymentOfferElement
                                , renderPrice
                                , acceptElement
                                , renderSubmit
                                , DOM.label {}
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
    }
  where
    renderPrice =
      DOM.label
        { className: "price"
        , children:
            [ DOM.span_ [ DOM.text "Ditt pris" ]
            , DOM.h3_ [ DOM.text $ formatEur offer.totalPrice <> " €"
                      , guard (paymentMethod == PaperInvoice) $
                          DOM.span_ [ DOM.text "+ 5 € faktureringstillägg per pappersfaktura" ]
                      ]
            , DOM.p
                { className: "total-price"
                , children:
                    [ DOM.strong_ [ DOM.text $ "Totalpris: " <>
                                    formatEur (offer.totalPrice +
                                               if paymentMethod == CreditCard then 0 else paperInvoiceCents) <>
                                    " €"
                                  ]
                    ]
                }
            ]
        }
    renderSubmit =
      DOM.input
        { id: "send-accpet-terms"
        , value: "Fortsätt"
        , type: "submit"
        , className: "submit-button"
        , disabled: not acceptTerms
        }
