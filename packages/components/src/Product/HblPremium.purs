module KSF.Product.HblPremium where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.NonEmpty as NonEmpty
import KSF.Api.Package (Package)
import KSF.Product as Product
import React.Basic (JSX)
import React.Basic.DOM as DOM

hblPremium :: Package -> JSX
hblPremium { name, id, offers } =
  let (product :: Product.Product) =
        { name: name
        , id:   id
        , description: "Alla artiklar på hbl.fi YOLO"
        , price: Array.head offers # _.monthlyPrice # toNumber # (_ / 100.0)
        }
  in Product.product product
