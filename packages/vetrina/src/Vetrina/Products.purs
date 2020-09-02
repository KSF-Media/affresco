module KSF.Products where

import Prelude

import Data.Array (intercalate)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic (JSX, fragment, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import Vetrina.Types (Product)

type Self = React.Self Props State
type State = {}
type Props =
  { products        :: Array Product
  , onProductChange :: Product -> Effect Unit
  , selectedProduct :: Maybe Product
  }

component :: React.Component Props
component = React.createComponent "Products"

product :: Props -> JSX
product = make component
  { initialState: {}
  , render
  }

render :: Self -> JSX
render self = DOM.div_ $ map renderProduct self.props.products
  where
    renderProduct p =
      DOM.div
        { className: "vetrina--product"
        , onClick: handler_ (self.props.onProductChange p)
        , children: [ DOM.input
                        { className: "vetrina--product-radio-button"
                        , type: "radio"
                        , name: "product"
                        , onChange: handler_ (self.props.onProductChange p)
                        , checked: case self.props.selectedProduct of
                          Just selectedProduct
                            | selectedProduct == p -> true
                            | otherwise -> false
                          Nothing -> false
                        }
                    , DOM.label
                        { htmlFor: "product"
                        , children:
                            [ DOM.div
                                { className: "vetrina--product-title"
                                , children: [ DOM.text p.id ]
                                }
                            , DOM.div
                              { className: "vetrina--product-description"
                              , children: [ showProductDescription p.description ]
                              }
                            ]
                        }
                    
                    ]
        }

    showProductDescription :: Array String -> JSX
    showProductDescription = DOM.div_ <<< Array.singleton <<< intercalate (DOM.br {}) <<< map DOM.text
