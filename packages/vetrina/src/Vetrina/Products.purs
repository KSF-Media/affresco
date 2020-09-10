module Vetrina.Products where

import Prelude

import Data.Array (intercalate)
import Data.Array as Array
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import React.Basic (JSX, make)
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
      DOM.label
        { className: "vetrina--product"
        , children: [ DOM.input $
                        { type: "radio"
                        , name: "vetrina-product-selection"
                        , onChange: handler_ (self.props.onProductChange p)
                        , checked: maybe false (eq p.id <<< _.id) self.props.selectedProduct
                        , value: p.id
                        }
                    , DOM.div
                        { className: "vetrina--product-radio-button"
                        , children:
                            [ DOM.div { className: "vetrina--product-radio-button_checked" } ]
                        }
                    , DOM.div_
                        [ DOM.div
                            { className: "vetrina--product-title"
                            , children: [ DOM.text p.id ]
                            }
                        , DOM.div
                            { className: "vetrina--product-description"
                            , children: [ showProductDescription p.description ]
                            }
                        ]
                    ]
        }

    showProductDescription :: Array String -> JSX
    showProductDescription = DOM.div_ <<< Array.singleton <<< intercalate (DOM.br {}) <<< map DOM.text
