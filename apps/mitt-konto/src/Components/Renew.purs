module MittKonto.Components.Renew where

import Prelude

import Bottega.Models (PaymentMethod(..))
import Data.Array (snoc)
import Data.Nullable as Nullable
import KSF.Api.Subscription as Subscription
import MittKonto.Main.UserView.Subscription.Types as Types
import Prenumerera.Package as Package
import Prenumerera.Page.SelectPeriod as SelectPeriod
import Prenumerera.Page.Payment as Payment
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useState', (/\))
import React.Basic.Hooks as React

data Stage = SelectPeriod | Payment Package.PackageOffer PaymentMethod | Finished

component :: Component Types.RenewSubscription
component = do
  selectPeriod <- SelectPeriod.component
  payment <- Payment.component
  React.component "Renew" $ \props@{package, description, user, onCancel} -> React.do
    stage /\ setStage <- useState' SelectPeriod
    let initialPaymentMethod =
          case props.subscription.paymentMethod of
            Subscription.CreditCard -> CreditCard
            Subscription.UnknownPaymentMethod -> CreditCard
            _ -> if props.user.address /= Nullable.null then PaperInvoice else CreditCard
        -- The default action of SelectPeriod is to ask for address if
        -- none is set and paper invoice is selected, but that makes
        -- less sense when used in Mitt Konto.
        availablePaymentMethods =
          if props.user.address /= Nullable.null then [ CreditCard, PaperInvoice ] else [ CreditCard ]
        closable content = DOM.div
          { className: "route-wrapper payment-popup"
          , children:
              [ DOM.div
                  { className: "header-x-button"
                  , children:
                      [ DOM.div
                          { className: "close-button"
                          , children: [ DOM.div { className: "close-icon" } ]
                          , onClick: handler_ onCancel
                          }
                      ]
                  }
              ] `snoc` content
          }

    pure $ case stage of
      SelectPeriod ->
        DOM.div
          { className: "route-wrapper"
          , children:
              [ DOM.div
                  { className: "close-button"
                  , children: [ DOM.div { className: "close-icon" } ]
                  , onClick: handler_ onCancel
                  }
              ]
          } <>
        selectPeriod
          { package
          , description
          , user
          , initialPaymentMethod
          , availablePaymentMethods
          , cancel: onCancel
          , next: \offer paymentMethod _ -> setStage $ Payment offer paymentMethod
          }
      Payment offer method ->
        closable $ payment
          { user
          , package
          , description
          , offer
          , method
          , next: setStage Finished
          }
      Finished ->
        closable $ DOM.text "Köpet är klart"
