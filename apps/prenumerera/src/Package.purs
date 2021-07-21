module Prenumerera.Package (module Prenumerera.Package, module ApiExport) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Maybe (Maybe)
import KSF.Api.Package (PackageId, PackageOffer)
import KSF.Api.Package as Api
import KSF.Api.Package (PackageId, PackageOffer) as ApiExport

-- Prenumerera can only make orders with offers, so use this to make
-- things a bit simpler

type Package =
  { id           :: PackageId
  , name         :: String
  , paper        :: { code :: String, name :: String }
  , offers       :: NonEmptyArray PackageOffer
  }

fromApiPackage :: Api.Package -> Maybe Package
fromApiPackage { id, name, paper, offers } =
  { id
  , name
  , paper
  , offers: _
  }
  <$> fromArray offers
