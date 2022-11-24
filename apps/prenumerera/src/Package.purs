module Prenumerera.Package (module Prenumerera.Package, module ApiExport) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Maybe (Maybe)
import KSF.Api.Package (PackageId, PackageOffer)
import KSF.Api.Package as Api
import KSF.Api.Package (PackageId, PackageOffer) as ApiExport
import KSF.Paper as Paper

-- Prenumerera can only make orders with offers, so use this to make
-- things a bit simpler

type Package =
  { id           :: PackageId
  , name         :: String
  , digitalOnly  :: Boolean
  , paper        :: Paper.Paper
  , offers       :: NonEmptyArray PackageOffer
  }

fromApiPackage :: Api.Package -> Maybe Package
fromApiPackage { id, name, paper, offers, digitalOnly } =
  { id
  , name
  , digitalOnly
  , offers: _
  , paper: _
  }
  <$> fromArray offers
  <*> Paper.fromString paper.code
