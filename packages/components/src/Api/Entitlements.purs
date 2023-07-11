module KSF.Api.Entitlements where

import Prelude
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Foreign (Foreign, ForeignError(..), fail)
import Foreign.Object (lookup)
import Simple.JSON (class ReadForeign, readImpl)

type AllowEntitlementsQuery =
  { endAt :: JSDate
  , onlyProducts :: Array String
  , startAt :: JSDate
  }

-- | The dates in a paywall opening record are Strings containing ISO 8601
--   dates. We only want to show those dates to the user so there's no point in
--   spending effort to convert them into more structured date type values.
data PaywallOpening = PaywallOpening
  { id :: Int
  , onlyToProducts :: Array String
  , startAt :: String
  , endAt :: String
  }

instance readPaywallOpening :: ReadForeign PaywallOpening where
  readImpl a = do
    obj <- readImpl a
    let results :: Maybe (Array Foreign)
        results = do
          w <- lookup "id" obj
          x <- lookup "onlyToProducts" obj
          y <- lookup "startAt" obj
          z <- lookup "endAt" obj
          pure [w, x, y, z]
    case results of
      Just [w, x, y, z] -> do
        id <- readImpl w
        onlyToProducts <- readImpl x
        startAt <- readImpl y
        endAt <- readImpl z
        pure $ PaywallOpening { id, onlyToProducts, startAt, endAt }
      _ ->
        fail (ForeignError "unable to process paywall opening")
