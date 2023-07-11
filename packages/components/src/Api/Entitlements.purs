module KSF.Api.Entitlements where

import Prelude
import Data.Maybe (Maybe)
import Data.JSDate (JSDate)

type AllowEntitlementsQuery =
  { endAt :: JSDate
  , onlyProducts :: Array String
  , startAt :: JSDate
  }

type PaywallOpening =
  { id :: Int
  , onlyToProducts :: Array String
  , startAt :: JSDate
  , endAt :: JSDate
  }
