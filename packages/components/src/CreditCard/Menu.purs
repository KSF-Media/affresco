module KSF.CreditCard.Menu where

import Prelude

import Bottega.Models (CreditCard)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import KSF.CreditCard.Menu.Item (item) as CreditCard
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM

type Self = React.Self Props State

type State = 
  { chosenCard :: Maybe CreditCard
  }

setChosenCard :: Maybe CreditCard -> State -> State
setChosenCard chosenCard = _ { chosenCard = chosenCard }

type Props = 
  { creditCards :: Array CreditCard
  , chosenCard  :: Maybe CreditCard
  }

menu :: Props -> JSX
menu = make component 
  { initialState
  , render
  , didMount 
  }

component :: React.Component Props
component = React.createComponent "menu"

initialState :: State
initialState = { chosenCard: Nothing }

didMount :: Self -> Effect Unit
didMount self@{ props: { chosenCard: Just card }, setState, state } =
  setState $ setChosenCard $ Just card
didMount _ = pure unit

render :: Self -> JSX
render self = DOM.div
                { className: "credit-card-menu--wrapper"
                , children: []
                }
  where
    creditCardsList :: JSX
    creditCardsList = DOM.div
                        { className: "credit-card-menu--list"
                        , children: map creditCardMenuItem self.props.creditCards
                        }
      where
        creditCardMenuItem :: CreditCard -> JSX
        creditCardMenuItem creditCard = CreditCard.item { creditCard: creditCard }