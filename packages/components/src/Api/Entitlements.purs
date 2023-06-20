module KSF.Api.Entitlements where

import Prelude
import Data.Maybe (Maybe)
import Data.JSDate (JSDate)

type AllowEntitlementsQuery =
  { byPackageId :: Maybe String
  , endAt :: JSDate
  , onlyProducts :: Array String
  , startAt :: JSDate
  }
