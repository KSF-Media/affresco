module KSF.Product.HblPremium where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.NonEmpty as NonEmpty
import KSF.Api.Package (Package)
import KSF.Product as Product
import React.Basic (JSX)
import React.Basic.DOM as DOM

hblPremium :: Package -> JSX
hblPremium package = Product.product $ toProduct package

toProduct :: Package -> Product.Product
toProduct  { name, id, offers } =
  { name: name
  , id:   id
  , description: "Alla artiklar på hbl.fi YOLO"
  , price: maybe 100.0 (_.monthlyPrice >>> toNumber >>> (_ / 100.0)) $ Array.head offers
  }
