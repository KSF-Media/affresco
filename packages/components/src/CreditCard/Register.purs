module KSF.CreditCard.Register where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import Web.HTML (window) as Web.HTML
import Web.HTML.Window as Window

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

scaRequired :: String -> JSX
scaRequired paymentTerminalUrl =
  let handleClick :: Effect Unit
      handleClick = do
        globalWindow <- Web.HTML.window
        _ <- Window.open paymentTerminalUrl "_blank" "noopener" globalWindow
        pure unit
  in DOM.div
    { className: "credit-card-register--wrapper"
    , children:
      [ DOM.text "Betalningen kräver ytterligare bekräftelse. Vänligen tryck på knappen nedan för att fortsätta."
      , DOM.div_
          [ DOM.button
              { className: "vetrina--button"
              , onClick: handler preventDefault $ const handleClick
              , children: [ DOM.text "Fortsätt" ]
              }
          ]
      ]
    }
