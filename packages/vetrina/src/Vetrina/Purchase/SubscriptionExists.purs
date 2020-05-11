module Vetrina.Purchase.SubscriptionExists where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Props =
  { onClose       :: Effect Unit
  }

subscriptionExists :: Props -> JSX
subscriptionExists props =
  DOM.h1_ [  DOM.text "You already have the subscription" ]
  <> DOM.p
       { className: "vetrina--description-text"
       , children: [ DOM.text "Yo, we noticed you are already entitled to read this." ]
       }
  <> closeButton props

closeButton :: Props -> JSX
closeButton props =
  DOM.button
    { className: "vetrina--button vetrina--completed-close"
    , children: [ DOM.text "Fortsätt läsa artikeln" ]
    , onClick: handler_ $ props.onClose
    }
