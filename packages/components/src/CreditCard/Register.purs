module KSF.CreditCard.Register where

import KSF.User (PaymentTerminalUrl)
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM

type Props =
  { terminalUrl :: PaymentTerminalUrl
  }

type Self = React.Self Props State

type State = {}

register :: Props -> JSX
register = make component { initialState: {}, render }

component :: React.Component Props
component = React.createComponent "Register"

render :: Self -> JSX
render self@{ props: { terminalUrl } } =
  DOM.div_ [ warning
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
