module Prenumerera.Page.Payment where

import Prelude

import Bottega (BottegaError(..))
import Bottega.Models (FailReason(..), OrderState(..), PaymentMethod(..), PaymentTerminalUrl)
import Bottega.Models.Order (OrderSource(..))
import Bottega.Poller as Poller
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Now as Now
import KSF.Spinner as Spinner
import KSF.User (User)
import KSF.User as User
import KSF.Window (close)
import Prenumerera.Package (Package, PackageOffer)
import Prenumerera.Package.Description (Description)
import Prenumerera.Summary as Summary
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useEffect, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Web.HTML as Web.HTML
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.Window as Window

type Props =
  { user :: User
  , package :: Package
  , description :: Description
  , offer :: PackageOffer
  , method :: PaymentMethod
  , next :: Effect Unit
  , window :: Maybe Window.Window
  }

type State =
  { netsUrl :: Maybe PaymentTerminalUrl
  , poller :: Poller.Poller
  }

component :: Component Props
component = do
  poller <- Poller.new
  today <- Now.nowDate
  globalWindow <- Web.HTML.window
  React.component "Payment" $ \ { user, package, description, offer, method, next, window } -> React.do
    orderState /\ setOrderState <- useState' $ Right OrderUnknownState
    netsUrl /\ setNetsUrl <- useState' Nothing
    error /\ setError <- useState' $ Right unit
    scaShown /\ setScaShown <- useState' false
    paperInvoiceConfirmed /\ setPaperInvoiceConfirmed <- useState' false
    let needTerminal = method == CreditCard &&
                       (error *> orderState) == Right OrderCreated &&
                       isJust netsUrl
        paymentUrl = (maybe "" _.paymentTerminalUrl netsUrl)
    useEffect needTerminal do
      when needTerminal $ case window of
        Just w -> do
          l <- Window.location w
          Web.HTML.Location.setHref paymentUrl l
        Nothing -> do
          void $ Window.open paymentUrl "_blank" "noopener" globalWindow
      pure $ pure unit
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
                for_ window close
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
    let orderSummary = Summary.render today user description offer method
    let cancel = Aff.launchAff_ do
           Poller.kill poller
           liftEffect $ setOrderState $ Right OrderCanceled
    pure $ case method of
      PaperInvoice -> case paperInvoiceConfirmed of
        false -> renderPaperInvoice orderSummary startPaperInvoicePurchase
        true -> renderPayment (error *> orderState) cancel scaShown setScaShown netsUrl
      CreditCard -> renderPayment (error *> orderState) cancel scaShown setScaShown netsUrl

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

renderPayment :: Either BottegaError OrderState -> Effect Unit -> Boolean -> (Boolean -> Effect Unit) -> Maybe PaymentTerminalUrl -> JSX
-- This should only happen during initialization
renderPayment (Right OrderUnknownState) _ _ _ _ = initializing
renderPayment (Right OrderCreated) cancel _ _ (Just _) =
  DOM.div
    { className: "payment-terminal payment-terminal-msg"
    , children:
        [ DOM.div_
            [ DOM.text "Betalningen öppnas i ett nytt fönster. Följ anvisningarna i det nya fönstret. Du kommer vidare till bekräftelsen när betalningen genomförts. Vid problem ta kontakt med vår kundtjänst på kundservice@hbl.fi."
            ]
        , DOM.div_
            [ DOM.button
                { children: [ DOM.text "Avbryt" ]
                , onClick: capture_ cancel
                }
            ]
        ]
    }
renderPayment (Right OrderCreated) _ _ _ _ = loading
renderPayment (Right OrderStarted) _ _ _ _ = loading
renderPayment (Right OrderCanceled) _ _ _ _ =
  errMsg "Beställningen avbrutits."
renderPayment (Right (OrderFailed NetsCanceled)) _ _ _ _ =
  errMsg "Beställningen avbrutits."
renderPayment (Right OrderScaRequired) _ false setScaShown (Just url) = scaRequired url setScaShown
renderPayment (Right OrderScaRequired) _ true _ _ = loading
renderPayment (Right (OrderFailed NetsIssuerError)) _ _ _ _ =
  errMsg "Ett fel uppstod vid Nets."
renderPayment (Right (OrderFailed SubscriptionExistsError)) _ _ _ _ =
  errMsg "Du har redan en prenumeration för denna produkt."
renderPayment (Left BottegaTimeout) _ _ _ _ =
  errMsg "Timeout hände."
renderPayment _ _ _ _ _ =
  errMsg "Något gick fel."

initializing :: JSX
initializing = Spinner.loadingSpinnerWithMessage "Vänligen vänta. Vi behandlar din beställning."

loading :: JSX
loading = Spinner.loadingSpinnerWithMessage "Vänligen vänta. Vi behandlar din beställning."

scaRequired :: PaymentTerminalUrl -> (Boolean -> Effect Unit) -> JSX
scaRequired { paymentTerminalUrl } setScaShown =
  let handleClick :: Effect Unit
      handleClick = do
        globalWindow <- Web.HTML.window
        _ <- Window.open paymentTerminalUrl "_blank" "noopener" globalWindow
        setScaShown true
  in DOM.div
    { className: "payment-terminal payment-terminal-msg"
    , children: [ DOM.div_ [ DOM.text "Betalningen kräver ytterligare bekräftelse. Vänligen tryck på knappen nedan för att fortsätta." ]
                , DOM.div_
                    [ DOM.button
                        { className: "btn btn-cta"
                        , onClick: handler preventDefault $ const handleClick
                        , children: [ DOM.text "Fortsätt" ]
                        }
                    , DOM.text "."
                    ]
                ]
    }

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
