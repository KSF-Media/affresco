module Prenumerera.Payment where

import Prelude

import Bottega (BottegaError(..))
import Bottega.Models (FailReason(..), OrderState(..), PaymentMethod(..), PaymentTerminalUrl)
--import Bottega.Models (PaymentMethod(..))
import Bottega.Poller as Poller
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Helpers (formatEur, paperInvoiceCents)
import KSF.Spinner as Spinner
import KSF.User (User)
import KSF.User as User
import KSF.User.Cusno as Cusno
import Prenumerera.Package (Package, PackageOffer)
import Prenumerera.PackageDescription (Description)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { user :: User
  , package :: Package
  , description :: Description
  , offer :: PackageOffer
  , method :: PaymentMethod
  , next :: Effect Unit
  }

type State =
  { netsUrl :: Maybe PaymentTerminalUrl
  , poller :: Poller.Poller
  }

component :: Component Props
component = do
  React.component "Payment" $ \ { user, package, description, offer, method, next } -> React.do
    poller /\ setPoller <- useState Poller.new
    netsUrl /\ setNetsUrl <- useState' Nothing
    error /\ setError <- useState' $ Right unit
    paperInvoiceConfirmed /\ setPaperInvoiceConfirmed <- useState' false
    let startPayOrder :: Effect Unit
        startPayOrder = do
          Aff.launchAff_ do
            eitherNetsUrl <- runExceptT do
              let newOrder =
                    { packageId: package.id
                    , period: offer.months
                    , payAmountCents: offer.totalPrice
                    , campaignNo: Nothing
                    }
              order <- ExceptT $ User.createOrder newOrder
              nets <- ExceptT $ User.payOrder order.number method
              pure { nets, order }
            case eitherNetsUrl of
              Left err -> liftEffect do
                setError $ Left err
              Right { nets, order } -> liftEffect do
                Poller.start poller setPoller order.number
                setNetsUrl nets
        startPaperInvoicePurchase :: Effect Unit
        startPaperInvoicePurchase = do
          setPaperInvoiceConfirmed true
          startPayOrder
    -- If using CreditCard, immediately create the order
    useEffectOnce do
      setPoller _ { onComplete = liftEffect do
                       case poller.orderState of
                         Right OrderCompleted -> next
                         _ -> setError $ Left $ BottegaUnexpectedError "Något gick fel."
                  }
      when (method == CreditCard) startPayOrder
      pure $ pure unit
    let orderSummary = renderOrderSummary user description offer method 
    pure $ case method of
      PaperInvoice -> case paperInvoiceConfirmed of
        false -> renderPaperInvoice orderSummary startPaperInvoicePurchase
        true -> renderPayment (error *> poller.orderState) netsUrl
      CreditCard -> renderPayment (error *> poller.orderState) netsUrl

renderPaperInvoice :: JSX -> Effect Unit -> JSX
renderPaperInvoice summary confirm =
  DOM.div
    { className: "container ksf-block confirm"
    , children:
        [ DOM.div
            { className: "row"
            , children:
                [ DOM.div
                    { className: "confirm-greeting"
                    , children:
                        [ DOM.h2_ [ DOM.text "Din prenumeration" ]
                        , DOM.p_ [ DOM.text "Granska uppgifterna nedan före du fortsätter" ]
                        , DOM.form
                            { onSubmit: handler preventDefault $ const confirm
                            , children:
                                [ DOM.input
                                    { type: "submit"
                                    , value: "Godkänn"
                                    }
                                ]
                            }
                        ]
                    }
                    , summary
                ]
            }
        ]
    }

renderOrderSummary :: User -> Description -> PackageOffer -> PaymentMethod -> JSX
renderOrderSummary user description offer method =
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
{-
                , prop "Adress" $
                  maybe [[], [], [], []]
                  (\address -> [ address.streetAddress
                               , fromMaybe "" address.zipCode
                               , fromMaybe "" address.city
                               , address.countryCode
                               ]) $ toMaybe user.address
-}
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

--       CreditCard -> renderLoading poller.orderState method netsUrl
renderPayment :: Either BottegaError OrderState -> Maybe PaymentTerminalUrl -> JSX
renderPayment (Right OrderCreated) (Just { paymentTerminalUrl }) =
  DOM.iframe
    { className: "payment-terminal"
    , src: paymentTerminalUrl
    }
renderPayment (Right OrderCreated) _ = loading
renderPayment (Right OrderStarted) _ = loading
renderPayment (Right OrderCanceled) _ = canceled
renderPayment (Right (OrderFailed NetsCanceled)) _ = canceled
renderPayment (Right (OrderFailed NetsIssuerError)) _ = issuerError
renderPayment _ _ =
  DOM.div
    { className: "payment-terminal"
    , children: [ DOM.text "Något gick fel." ]
    }

loading :: JSX
loading = Spinner.loadingSpinnerWithMessage "Vänligen vänta. Vi behandlar din beställning."

canceled :: JSX
canceled =
  DOM.div
    { className: "payment-terminal"
    , children: [ DOM.text "canceled" ]
    }

issuerError :: JSX
issuerError =
  DOM.div
    { className: "payment-terminal"
    , children: [ DOM.text "issuer" ]
    }

{-
        startPay offer method = do
          case purchasePackage of
            Nothing -> do
              error "Purchase with no package - should be impossible"
            Just (Tuple package _) -> do
              Aff.launchAff_ do
                eitherNetsUrl <- runExceptT do
                  let newOrder =
                        { packageId: package.id
                        , period: offer.months
                        , payAmountCents: offer.totalPrice
                        , campaignNo: Nothing
                        }
                  order <- ExceptT $ User.createOrder newOrder
                  nets <- ExceptT $ User.payOrder order.number method
                  pure { nets, order }
                case eitherNetsUrl of
                  Left BottegaInsufficientAccount -> do
                    error "insufficient account"
                  Left (BottegaUnexpectedError err) -> liftEffect do
                    error "unexpected error"
                  Right { nets, order } -> liftEffect do
                    
-}
