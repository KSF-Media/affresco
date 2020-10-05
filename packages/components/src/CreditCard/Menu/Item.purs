module KSF.CreditCard.Menu.Item where

import Prelude

import Bottega.Models (CreditCard)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
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
  { selected :: Boolean
  }

type Props = 
  { creditCard :: CreditCard
  }

item :: Props -> JSX
item = make component { initialState, render }

component :: React.Component Props
component = React.createComponent "item"

initialState :: State
initialState = { selected: false }

render :: Self -> JSX
render self = DOM.div
                { className: wrapperClass
                , children: [ number "1"
                            , expiryDate "05/2020"
                            ]
                }
  where
    wrapperClass :: String
    wrapperClass = if self.state.selected 
                   then 
                     "credit-card-menu-item--wrapper"
                   else
                     "credit-card-menu-item--wrapper-selected"

    number :: String -> JSX
    number n = DOM.div 
                 { className: "credit-card-menu-item--number"
                 , children: [ DOM.text n ]
                 }
    
    expiryDate :: String -> JSX
    expiryDate d = DOM.div 
                     { className: "credit-card-menu-item--expiry-date"
                     , children: [ DOM.text d ]
                     }