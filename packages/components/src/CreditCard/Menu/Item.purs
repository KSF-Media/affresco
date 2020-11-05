module KSF.CreditCard.Menu.Item where

import Bottega.Models (CreditCard)
import React.Basic as React
import React.Basic (JSX, make)
import React.Basic.DOM as DOM

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
render self@{ state, props: { creditCard } } = DOM.div
                { className: wrapperClass
                , children: [ number creditCard.maskedPan
                            , expiryDate creditCard.expiryDate
                            ]
                }
  where
    wrapperClass :: String
    wrapperClass = if state.selected 
                   then 
                     "credit-card-menu-item--wrapper"
                   else
                     "credit-card-menu-item--wrapper selected"

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