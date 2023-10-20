module Prenumerera.Page.Payment where

import Prelude

import Bottega (BottegaError(..), IdentificationError(..))
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
import KSF.Spinner as Spinner
import KSF.User (User)
import KSF.User as User
import KSF.Window (close)
import Prenumerera.Identification as Identification
import Prenumerera.Package (Package, PackageOffer)
import Prenumerera.Package.Description (Description)
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

data InteractionRequired
  = NetsTerminalUse (Maybe PaymentTerminalUrl)
  | StrongIdentification JSX

component :: Component Props
component = do
  poller <- Poller.new
  globalWindow <- Web.HTML.window
  identificationComponent <- Identification.component globalWindow
  React.component "Payment" $ \ { user, package, offer, method, next, window } -> React.do
    orderState /\ setOrderState <- useState' $ Right OrderUnknownState
    orderNum /\ setOrderNum <- useState' Nothing
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

    -- Starts the same for card and invoice payments.
    useEffectOnce do
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
            liftEffect do
              setNetsUrl nets
              setOrderNum $ Just order.number
      pure $ Aff.launchAff_ $ Poller.kill poller

    -- Only for invoice payments.
    let identification =
          identificationComponent
            { user
            , setError: setError <<< Left <<< BottegaIdentificationError
            , next: case orderNum of
              Nothing -> do
                setOrderState $ Left $ BottegaIdentificationError $
                  StrongIdentificationFailed "no order number set"
              Just num -> do
                setPaperInvoiceConfirmed true
                Aff.launchAff_ do
                  ver <- User.userVerified num
                  case ver of
                    -- The order poller will see it succeed
                    Right _ -> pure unit
                    Left err -> liftEffect $ setOrderState $ Left err
            }
    let interactionRequired = case method of
          CreditCard -> Just (NetsTerminalUse netsUrl)
          PaperInvoice | not paperInvoiceConfirmed -> Just (StrongIdentification identification)
          _ -> Nothing

    useEffect orderState do
      when (orderState == Right OrderCompleted) next
      pure $ pure unit
    let cancel = Aff.launchAff_ do
           Poller.kill poller
           liftEffect $ setOrderState $ Right OrderCanceled
    pure $ renderPayment (error *> orderState) cancel scaShown setScaShown interactionRequired

renderPayment :: Either BottegaError OrderState -> Effect Unit -> Boolean -> (Boolean -> Effect Unit) -> Maybe InteractionRequired -> JSX
-- This should only happen during initialization
renderPayment (Right OrderUnknownState) _ _ _ _ = initializing
renderPayment (Right OrderCreated) cancel _ _ (Just interaction) =
  DOM.div
    { className: "payment-terminal payment-terminal-msg"
    , children:
        [ DOM.div_ [ DOM.text paymentMessage ]
        , case interaction of
            StrongIdentification subComponent -> subComponent
            _ -> mempty
        , DOM.div_
            [ DOM.button
                { children: [ DOM.text "Avbryt" ]
                , onClick: capture_ cancel
                }
            ]
        ]
    }
  where
    paymentMessage = case interaction of
      NetsTerminalUse _ -> "Betalningen öppnas i ett nytt fönster. Följ anvisningarna i det nya fönstret. Du kommer vidare till bekräftelsen när betalningen genomförts. Vid problem ta kontakt med vår kundtjänst på kundservice@hbl.fi."
      StrongIdentification _ -> "Identifikation öppnas i ett nytt fönster. Följ anvisningarna i det nya fönstret. Du kommer vidare till bekräftelsen när identification genomförts. Vid problem ta kontakt med vår kundtjänst på kundservice@hbl.fi."
renderPayment (Right OrderCreated) _ _ _ _ = loading
renderPayment (Right OrderStarted) _ _ _ _ = loading
renderPayment (Right OrderCanceled) _ _ _ _ =
  errMsg "Beställningen avbrutits."
renderPayment (Right (OrderFailed NetsCanceled)) _ _ _ _ =
  errMsg "Beställningen avbrutits."
renderPayment (Right OrderScaRequired) _ false setScaShown (Just (NetsTerminalUse (Just url))) = scaRequired url setScaShown
renderPayment (Right OrderScaRequired) _ true _ _ = loading
renderPayment (Right (OrderFailed NetsIssuerError)) _ _ _ _ =
  errMsg "Ett fel uppstod vid Nets."
renderPayment (Right (OrderFailed SubscriptionExistsError)) _ _ _ _ =
  errMsg "Du har redan en prenumeration för denna produkt."
renderPayment (Left BottegaTimeout) _ _ _ _ =
  errMsg "Timeout hände."
renderPayment (Left (BottegaIdentificationError StrongIdentificationWindowOpenFailed)) _ _ _ _ =
  errMsg "Popup blocked?"
renderPayment (Left (BottegaIdentificationError (StrongIdentificationFailed _))) _ _ _ _ =
  errMsg "Identification failed"
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
