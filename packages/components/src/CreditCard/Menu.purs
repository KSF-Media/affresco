module KSF.CreditCard.Menu where

import Prelude

import Bottega.Models (CreditCard)
import Effect (Effect)
import KSF.CreditCard.Menu.Item (item) as CreditCard
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React

type Props =
  { creditCards :: Array CreditCard
  , onSelect :: CreditCard -> Effect Unit
  }

component :: Component Props
component = React.component "menu" $ pure <<< render

render
  :: Props
  -> JSX
render { creditCards, onSelect } =
  DOM.div
    { className: "credit-card-menu"
    , children:  map creditCardMenuItem creditCards
    }
  where
    creditCardMenuItem :: CreditCard -> JSX
    creditCardMenuItem creditCard = CreditCard.item
      { creditCard: creditCard
      , onClick: onSelect creditCard
      }
