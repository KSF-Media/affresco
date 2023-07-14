module KSF.Api.Entitlements where

import Data.Nullable (Nullable)

type AllowEntitlementsQuery =
  { onlyToProducts :: Array String
  , startAt :: String
  , endAt :: String
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
