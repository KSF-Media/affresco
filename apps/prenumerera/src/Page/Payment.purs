module Prenumerera.Page.Payment where

import Prelude

import Bottega (BottegaError(..))
import Bottega.Models (FailReason(..), OrderState(..), PaymentMethod(..), PaymentTerminalUrl)
import Bottega.Models.Order (OrderSource(..))
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
import Prenumerera.Package.Description (Description)
import Prenumerera.Summary as Summary
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useEffect, useEffectOnce, useState', (/\))
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
  poller <- Poller.new
  React.component "Payment" $ \ { user, package, description, offer, method, next } -> React.do
    orderState /\ setOrderState <- useState' $ Right OrderUnknownState
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
                    , orderSource: Just PrenumereraSource
                    }
              order <- ExceptT $ User.createOrder newOrder
              nets <- ExceptT $ User.payOrder order.number method
              pure { nets, order }
            case eitherNetsUrl of
              Left err -> liftEffect do
                setError $ Left err
              Right { nets, order } -> do
                Poller.startOrder poller setOrderState order.number
                liftEffect $ setNetsUrl nets
        startPaperInvoicePurchase :: Effect Unit
        startPaperInvoicePurchase = do
          setPaperInvoiceConfirmed true
          startPayOrder
    -- If using CreditCard, immediately create the order
    useEffectOnce do
      when (method == CreditCard) startPayOrder
      pure $ Aff.launchAff_ $ Poller.kill poller
    useEffect orderState do
      when (orderState == Right OrderCompleted) next
      pure $ pure unit
    let orderSummary = Summary.render user description offer method
    pure $ case method of
      PaperInvoice -> case paperInvoiceConfirmed of
        false -> renderPaperInvoice orderSummary startPaperInvoicePurchase
        true -> renderPayment (error *> orderState) netsUrl
      CreditCard -> renderPayment (error *> orderState) netsUrl

renderPaperInvoice :: JSX -> Effect Unit -> JSX
renderPaperInvoice summary confirm =
  DOM.div
    { className: "container ksf-block confirm"
    , children:
        [ DOM.div
            { className: "row"
            , children:
                [ DOM.div
                    { className: "confirm-greeting ksf-form-container"
                    , children:
                        [ DOM.h2_ [ DOM.text "Din prenumeration" ]
                        , DOM.p_ [ DOM.text "Granska uppgifterna nedan före du fortsätter" ]
                        , DOM.form
                            { onSubmit: handler preventDefault $ const confirm
                            , className: "ksf-form"
                            , children:
                                [ DOM.input
                                    { type: "submit"
                                    , className: "submit-button"
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
-- This should only happen during initialization
renderPayment (Right OrderUnknownState) _ = initializing
renderPayment (Right OrderCreated) (Just { paymentTerminalUrl }) =
  DOM.iframe
    { className: "payment-terminal"
    , src: paymentTerminalUrl
    }
renderPayment (Right OrderCreated) _ = loading
renderPayment (Right OrderStarted) _ = loading
renderPayment (Right OrderCanceled) _ =
  errMsg "Beställningen avbrutits."
renderPayment (Right (OrderFailed NetsCanceled)) _ =
  errMsg "Beställningen avbrutits."
renderPayment (Right (OrderFailed NetsIssuerError)) _ =
  errMsg "Ett fel uppstod vid Nets."
renderPayment (Right (OrderFailed SubscriptionExistsError)) _ =
  errMsg "Du har redan en prenumeration för denna produkt."
renderPayment (Left BottegaTimeout) _ =
  errMsg "Timeout hände."
renderPayment _ _ =
  errMsg "Något gick fel."

initializing :: JSX
initializing = Spinner.loadingSpinnerWithMessage "Vänligen vänta. Vi behandlar din beställning."

loading :: JSX
loading = Spinner.loadingSpinnerWithMessage "Vänligen vänta. Vi behandlar din beställning."

errMsg :: String -> JSX
errMsg msg =
  DOM.div
    { className: "payment-terminal payment-terminal-msg"
    , children: [ DOM.div_ [ DOM.text msg ]
                , DOM.div_
                    [ DOM.a { href: "/", children: [ DOM.text "Tillbaka till startsidan" ] }
                    , DOM.text "."
                    ]
                ]
    }
