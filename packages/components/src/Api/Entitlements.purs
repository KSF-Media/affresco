module KSF.Api.Entitlements where

import Prelude
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Foreign (Foreign, ForeignError(..), fail)
import Foreign.Object (lookup)
import Simple.JSON (class ReadForeign, readImpl)

type AllowEntitlementsQuery =
  { endAt :: JSDate
  , onlyToProducts :: Array String
  , startAt :: JSDate
  }

-- | The dates in a paywall opening record are Strings containing ISO 8601
--   dates. We only want to show those dates to the user so there's no point in
--   spending effort to convert them into more structured date type values.
type PaywallOpening =
  { id :: Int
  , onlyToProducts :: Nullable (Array String)
  , startAt :: String
  , endAt :: String
  }
