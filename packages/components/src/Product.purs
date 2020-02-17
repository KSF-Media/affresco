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

-- TODO: Fetch these from server
hblPremium :: Product
hblPremium =
  { name: "HBL Premium"
  , id: "HBL WEBB"
  , description: "Alla artiklar på hbl.fi"
  , price: 6.9
  }

productOption :: Product -> Boolean -> JSX
productOption product checked =
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
                , checked
                }
            ]
        , DOM.label
            { htmlFor: "hbl-premium"
            , children: [ productInfo product ]
            }
        ]
    }

productInfo :: Product -> JSX
productInfo product =
  DOM.div
    { className: "product--product-info"
    , children:
        [ DOM.h1_ [ DOM.text product.name ]
        , DOM.p_  [ DOM.text product.description ]
        , DOM.p_  [ DOM.text $ show product.price ]
        ]
    }
