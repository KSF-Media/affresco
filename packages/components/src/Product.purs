module KSF.Product where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import KSF.Api.Package (Package, PackageName(..), PackageValidationError(..))
import KSF.Api.Package as Package
import React.Basic (JSX)
import React.Basic.DOM as DOM

type Product =
  { name        :: String
  , id          :: String
  , description :: String
  , price       :: Number
  }

productRender :: Product -> JSX
productRender p =
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

toProduct :: Array Package -> PackageName -> Either PackageValidationError Product
toProduct packages packageName = do
  validPackage <- Package.validatePackage packageName =<< Package.findPackage packageName packages
  -- TODO: Can we take price from the first offer?
  case Array.head validPackage.offers of
    Just { monthlyPrice } -> Right $
      { name: validPackage.name
      , id: validPackage.id
      , description: productDescription packageName
      , price: (toNumber monthlyPrice) / 100.0
      }
    _ -> Left $ PackageOffersMissing packageName

productDescription :: PackageName -> String
productDescription HblPremium =  "Alla artiklar på hbl.fi"
