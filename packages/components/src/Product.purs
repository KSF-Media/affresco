module KSF.Product where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM

type Product =
  { name        :: String
  , id          :: String
  , description :: String
  , price       :: Number
  }

product :: Product -> JSX
product p =
  DOM.div
    { className: "product--container"
    , children:
        [ -- We need a div wrapper here because the radio button
          -- would look weird if it's directly a grid child
          DOM.div_
            [ DOM.input
                { type: "radio"
                , name: "product"
                , id: "hbl-premium"
                , value: "hbl-premium"
   --             , checked
                }
            ]
        , DOM.label
            { htmlFor: "hbl-premium"
            , children: [ productInfo p ]
            }
        ]
    }

productInfo :: Product -> JSX
productInfo p =
  DOM.div
    { className: "product--product-info"
    , children:
        [ DOM.h1_ [ DOM.text p.name ]
        , DOM.p_  [ DOM.text p.description ]
        , DOM.p_  [ DOM.text $ show p.price ]
        ]
    }
