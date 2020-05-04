module Vetrina.Purchase.Error where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Props =
  { onRetry :: Effect Unit
  }

error :: Props -> JSX
error props =
  DOM.h1_ [ DOM.text "Något gick fel med ditt inköp!"
          ]
  <> DOM.p
       { className: "vetrina--description-text"
       , children: [ DOM.text "Klicka nedan för pröva på nytt eller ta kontakt med vår kundservice på "
                   , DOM.b_ [ DOM.text "pren@hbl.fi"
                           ]
                   , DOM.text "."
                   ]
       }
  <> DOM.p
       { className: "vetrina--description-text"
       , children: [ DOM.text "Vi beklagar strulet!"
                   ]
       }
  <> retryButton props

retryButton :: Props -> JSX
retryButton props =
  DOM.button
    { className: "vetrina--button vetrina--completed-close"
    , children: [ DOM.text "Pröva på nytt" ]
    , onClick: handler_ $ props.onRetry
    }
