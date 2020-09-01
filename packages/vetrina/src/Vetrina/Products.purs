module KSF.Products where

import Prelude

import Data.Array (intercalate)
import Data.Array as Array
import React.Basic (JSX, fragment, make)
import React.Basic as React
import React.Basic.DOM as DOM
import Vetrina.Types (Product)

type Self = React.Self Props State
type State = {}
type Props =
  { products :: Array Product
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
        , children: [ DOM.div
                        { className: "vetrina--product-radio-button"
                        , children: [ DOM.text "RADIO!" ]
                        }
                    , DOM.div
                        { className: "vetrina--product-title"
                        , children: [ DOM.text p.id ]
                        }
                    , DOM.div
                        { className: "vetrina--product-description"
                        , children: [ showProductDescription p.description ]
                        }
                    ]
        }

    showProductDescription :: Array String -> JSX
    showProductDescription = DOM.div_ <<< Array.singleton <<< intercalate (DOM.br {}) <<< map DOM.text
