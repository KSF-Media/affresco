module KSF.CreditCard.Menu where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import KSF.Bottega.Models (CreditCard)
import KSF.CreditCard.Menu.Item (item)
import KSF.Modal as Modal
import KSF.Spinner as Spinner
import KSF.User (PaymentTerminalUrl(..))
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

type Self = React.Self Props State

type State = 
  { chosenCard :: Maybe CreditCard
  }

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

initialState :: State
initialState = { chosenCard: Nothing }

didMount :: Self -> Effect Unit
didMount self@{ props: { chosenCard: Just card } } =
  pure self.setState _ { chosenCard: Just card }
didMount _ = pure unit

render :: Self -> JSX
render self = DOM.div
                { className: "credit-card-menu--wrapper"
                , children: []
                }
  where
    creditCardsList :: JSX
    creditCardSList = DOM.div
                        { className: "credit-card-menu--list"
                        , children: map creditCardItem self.props.creditCards
                        }