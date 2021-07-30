module Prenumerera.Payment where

import Prelude

import Bottega (BottegaError(..))
import Bottega.Models (FailReason(..), OrderState(..), PaymentMethod(..), PaymentTerminalUrl)
import Bottega.Poller as Poller
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Spinner as Spinner
import KSF.User (User)
import KSF.User as User
import Prenumerera.Package (Package, PackageOffer)
import Prenumerera.PackageDescription (Description)
import Prenumerera.Summary as Summary
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
    let orderSummary = Summary.render user description offer method
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
