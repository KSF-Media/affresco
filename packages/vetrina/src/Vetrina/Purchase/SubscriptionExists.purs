module Vetrina.Purchase.SubscriptionExists where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Props =
  { onClose :: Effect Unit
  , extraMsg :: JSX
  }

subscriptionExists :: Props -> JSX
subscriptionExists props =
  DOM.div
    { className: "vetrina--subscription-exists"
    , children:
       [ DOM.h1
           { className: "vetrina--headline"
           , children: [ DOM.text "Du har redan en prenumeration" ]
           }
       , props.extraMsg
       , closeButton props
       ]
    }

closeButton :: Props -> JSX
closeButton props =
  DOM.button
    { className: "vetrina--button vetrina--completed-close"
    , children: [ DOM.text "Fortsätt läsa artikeln" ]
    , onClick: handler_ props.onClose
    }
