module KSF.CreditCard.Menu where

import Prelude

import Bottega.Models (CreditCard)
import Effect (Effect)
import KSF.CreditCard.Menu.Item as CreditCard
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React

type Props =
  { creditCards :: Array CreditCard
  , onSelect :: CreditCard -> Effect Unit
  }

component :: Component Props
component = do
  creditCardMenuItemComponent <- CreditCard.component
  React.component "menu" $ \props -> React.do
    let creditCardMenuItem onSelect creditCard =
          creditCardMenuItemComponent { creditCard, onClick: onSelect creditCard }
        creditCardMenuItems = map
          (creditCardMenuItem props.onSelect)
          props.creditCards
    pure $ render creditCardMenuItems

render :: Array JSX -> JSX
render creditCardMenuItems =
  DOM.div
    { className: "credit-card-menu"
    , children:  creditCardMenuItems
    }
