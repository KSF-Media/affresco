module KSF.CreditCard.Menu.Item where

import Prelude

import Bottega.Models (CreditCard, CreditCardId(..))
import Data.String.CodePoints (splitAt)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React

type Props =
  { creditCard :: CreditCard
  , onClick :: Effect Unit
  }

component :: Component Props
component = React.component "item" $ \props -> React.do
  pure $ render props

render :: Props -> JSX
render { creditCard, onClick } =
  DOM.label
    { className: "credit-card-menu-item"
    , children:
        [ DOM.input
            { type: "radio"
            , name: "credit-card-menu-item--selection"
            , onClick: handler_ onClick
            , value: creditCardId creditCard.id
            }
        , DOM.div
            { className: "credit-card-menu-item--radio-button"
            , id: creditCardId creditCard.id
            , children:
                [ DOM.div { className: "credit-card-menu-item--radio-button_checked" } ]
            }
        , DOM.div_
            [ number creditCard.maskedPan
            , expiryDate creditCard.expiryDate
            ]
        ]
    }
  where
    creditCardId :: CreditCardId -> String
    creditCardId (CreditCardId id) = show id

    number :: String -> JSX
    number n = DOM.h4
                 { className: "credit-card-menu-item--number"
                 , children: [ DOM.text n ]
                 }

    expiryDate :: String -> JSX
    expiryDate d =
      DOM.div
        { className: "credit-card-menu-item--expiry-date"
        , children: [ DOM.text $ "Giltighetstid (månad/år): " <> formattedDate ]
        }
      where
        formattedDate :: String
        formattedDate =
          let { before: year, after: month } = splitAt 2 d
           in month <> "/" <> year
