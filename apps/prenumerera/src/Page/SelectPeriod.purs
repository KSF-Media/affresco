module Prenumerera.Page.SelectPeriod where

import Prelude

import Bottega.Models (PaymentMethod(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.List.NonEmpty (all)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Validation.Semigroup (toEither)
import Effect (Effect)
import KSF.Helpers (formatEur, paperInvoiceCents)
import KSF.InputField.Checkbox as Checkbox
import KSF.Paper as Paper
import KSF.Registration.Component (RegistrationInputField(..))
import KSF.Registration.Component as Registration
import KSF.User (User)
import KSF.ValidatableForm (isNotInitialized)
import Prenumerera.Package (Package, PackageOffer)
import Prenumerera.Package.Description (Description)
import Prenumerera.Page.Register as Register
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue, preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useState, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { package :: Package
  , description :: Description
  , user :: User
  , next :: PackageOffer -> PaymentMethod -> User -> Effect Unit
  , cancel :: Effect Unit
  }

component :: Component Props
component = do
  React.component "SelectPeriod" $ \ { package, description, user, next } -> React.do
    let initial = _.form $ Register.initialRegisterData false $ Just user
    offer /\ setOffer <- useState' $ head package.offers
    remind /\ setRemind <- useState' false
    paymentMethod /\ setPaymentMethod <- useState' CreditCard
    acceptTerms /\ setAcceptTerms <- useState' false
    form /\ setForm <- useState initial
    updateUserError /\ setUpdateUserError <- useState' false
    let setFormData f = setForm $ \s -> s { formData = f s.formData }
        confirm o m = case paymentMethod of
          PaperInvoice -> Register.updateUser
                          initial.formData setFormData
                          (setUpdateUserError true) (next o m) user $
                          Registration.formValidations form
          CreditCard -> next o m user
    let remindElement = guard remind renderRemind
        paymentOfferElement = renderPaymentOffer package.offers setOffer paymentMethod setPaymentMethod
        acceptElement = renderAccept acceptTerms setAcceptTerms
        addressElement = guard (paymentMethod == PaperInvoice) $ renderAddress package form setForm
    pure $ render description remindElement paymentOfferElement addressElement acceptElement
      form offer paymentMethod acceptTerms updateUserError $
      if acceptTerms then confirm offer paymentMethod else setRemind true

renderRemind :: JSX
renderRemind =
  DOM.div
    { className: "alert alert-warning accept-terms--narrow"
    , children:
        [ DOM.p_
            [ DOM.strong_ [ DOM.text "Hoppsan!" ]
            , DOM.br {}
            , DOM.text "Vänligen godkänn KSF Medias bruksvillkor för att fortsätta."
            ]
        ]
    }

renderPaymentOffer :: NonEmptyArray PackageOffer -> (PackageOffer -> Effect Unit) -> PaymentMethod -> (PaymentMethod -> Effect Unit) -> JSX
renderPaymentOffer offers setOffer paymentMethod setPaymentMethod =
  DOM.div
    { className: "payment accept-terms--narrow"
    , children:
        [ DOM.label_
            [ DOM.span_ [ DOM.text "Betalningssätt" ]
            , DOM.select
                { id: "payment_option"
                , name: "payment_option"
                , required: true
                , onChange: handler targetValue $ setPaymentMethod <<< decodeMethod
                , defaultValue: paymentOptionText paymentMethod
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
                , defaultValue: show $ _.months $ head offers
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
                      , DOM.span_ [ DOM.text "På pappersfakturor som levereras per post uppbär vi en tilläggsavgift på 5,00 euro per faktura (inkl. Moms). Du kan i din nätbank byta fakturan till en e-faktura som inte har ett faktureringstillägg. " ]
                      ]
                  ]
              }
        ]
    }
  where
    paymentOptionText CreditCard = "Kreditkort"
    paymentOptionText PaperInvoice = "Faktura"

    renderPaymentOptionOption option =
      DOM.option
        { value: paymentOptionText option
        , children: [ DOM.text $ paymentOptionText option ]
        }
    decodeMethod (Just "Faktura") = PaperInvoice
    decodeMethod _ = CreditCard
    renderOfferOption { months } =
      DOM.option
        { value: show months
        , children: [ DOM.text $ show months <> " " <>
                      if months == 1 then "månad" else "månader"
                    ]
        }

renderAccept :: Boolean -> (Boolean -> Effect Unit) -> JSX
renderAccept accept setAccept =
  DOM.div
    { className: "accept-terms--narrow"
    , children:
        [ Checkbox.inputCheckbox
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
        ]
    }

renderAddress :: Package -> Registration.State -> ((Registration.State -> Registration.State) -> Effect Unit) -> JSX
renderAddress { digitalOnly } form setForm = guard digitalOnly $
  DOM.div
    { className: "accept-terms--wide"
    , children:
        [ DOM.div
            { className: "row"
            , children: [ DOM.text "Vänligen kontrollera din adress för pappersfaktura." ]
            }
        , row [ inputField StreetAddress, inputField City ]
        , row [ inputField (Zip (form.formData.country)), inputField Country ]
        ]
    }
  where
    row xs =
      DOM.div
        { className: "row"
        , children: xs
        }
    inputField field = Registration.inputField field form setForm

render :: Description -> JSX -> JSX -> JSX -> JSX -> Registration.State -> PackageOffer -> PaymentMethod -> Boolean -> Boolean -> Effect Unit -> JSX
render description remindElement paymentOfferElement addressElement acceptElement form offer paymentMethod acceptTerms updateUserError submit =
  DOM.div
    { className: "container ksf-identify"
    , children:
        [ DOM.div
            { className: "row ksf-auth-wrapper"
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
                                    [ DOM.text "Välj betalningssätt och faktureringsperiod för din produkt "
                                    , DOM.strong_
                                        [ DOM.text $ "\"" <> descriptionText <> "\"" ]
                                    ]
                                , DOM.p_
                                    [ DOM.text "Prenumerationstyp: "
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
                                , addressElement
                                , renderPrice
                                , acceptElement
                                , renderSubmit
                                , DOM.label {}
                                ]
                            }
                        , guard updateUserError $
                          DOM.div
                            { className: "error-text"
                            , children: [ DOM.text "Någonting gick fel med uppdatering av adress." ]
                            }
                        ]
                    }
                ]
            }
        ]
    }
  where
    descriptionText =
      case description.descriptionShort of
        "" -> description.brandLong
        short -> (Paper.toString description.brand) <> short
    renderPrice =
      DOM.label
        { className: "price accept-terms--narrow"
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
        , disabled: not acceptTerms || (paymentMethod == PaperInvoice && isFormInvalid)
        }
    isFormInvalid
      | Left errs <- toEither $ Registration.formValidations form
      = not $ all isNotInitialized errs
      | otherwise = false
