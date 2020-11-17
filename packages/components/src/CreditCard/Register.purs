module KSF.CreditCard.Register where

import KSF.User (PaymentTerminalUrl)
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM

type Props = 
  { title :: JSX
  , terminalUrl :: PaymentTerminalUrl 
  }

type Self = React.Self Props State

type State = {}

register :: Props -> JSX
register = make component { initialState: {}, render }

component :: React.Component Props
component = React.createComponent "Register"

render :: Self -> JSX
render self@{ props: { title, terminalUrl } } = 
  DOM.div_ [ title
           , warning
           , netsTerminalIframe terminalUrl
           ]
  where
    warning :: JSX
    warning = DOM.div 
      { className: "credit-card-register--warning"
      , children: [ DOM.text "På kortet görs en reservation på en euro för att bekräfta att kortet är giltigt. Den här summan debiteras inte från kortet." ]
      }

    netsTerminalIframe :: PaymentTerminalUrl -> JSX
    netsTerminalIframe { paymentTerminalUrl } =
      DOM.div 
        { className: "credit-card-register--wrapper"
        , children : [ DOM.iframe
                         { src: paymentTerminalUrl
                         , className: "credit-card-register--terminal"
                         }
                     ]
        }