module KSF.CreditCard.Menu where

import Prelude

import Bottega.Models (CreditCard)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import KSF.CreditCard.Menu.Item (item) as CreditCard
import KSF.Grid as Grid
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)

type Self = React.Self Props State

type State = 
  { chosenCard :: Maybe CreditCard
  }

type Props = 
  { creditCards :: Array CreditCard
  , onSelect :: CreditCard -> Effect Unit
  }

menu :: Props -> JSX
menu = make component 
  { initialState
  , render
  }

component :: React.Component Props
component = React.createComponent "menu"

initialState :: State
initialState = { chosenCard: Nothing }

render :: Self -> JSX
render self@{ setState, state: { chosenCard }, props: { creditCards, onSelect } } = 
  DOM.div
    { className: "credit-card-menu--wrapper"
    , children: [ creditCardsForm ]
    }
  where
    creditCardsForm :: JSX
    creditCardsForm = DOM.div
                        { className: "credit-card-menu--list"
                        , children:  map creditCardMenuItem creditCards 
                        }

    creditCardMenuItem :: CreditCard -> JSX
    creditCardMenuItem creditCard = CreditCard.item 
      { creditCard: creditCard
      , onClick: onSelect creditCard
      }