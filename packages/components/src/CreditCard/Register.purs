module KSF.CreditCard.Register where

import React.Basic (JSX)
import React.Basic.DOM as DOM

render :: JSX
render =
  DOM.div_ [ warning
           , netsMessage
           ]
  where
    warning :: JSX
    warning = DOM.div
      { className: "credit-card-register--warning"
      , children: [ DOM.text "På kortet görs en reservation på en euro för att bekräfta att kortet är giltigt. Den här summan debiteras inte från kortet." ]
      }

    netsMessage :: JSX
    netsMessage =
      DOM.div
        { className: "credit-card-register--wrapper"
        , children : [ DOM.text "Registering öppnas i ett nytt fönster. Följ anvisningarna i det nya fönstret. Du kommer vidare till bekräftelsen när registering genomförts. Vid problem ta kontakt med vår kundtjänst på pren@ksfmedia.fi." ]
        }
