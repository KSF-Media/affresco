module KSF.Api.Address where

import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)

type Address =
  { countryCode   :: String
  , zipCode       :: Maybe String
  , city          :: Maybe String
  , streetAddress :: String
  }

type JSAddress =
  { countryCode   :: String
  , zipCode       :: Nullable String
  , city          :: Nullable String
  , streetAddress :: String
  }

fromJSAddress :: JSAddress -> Address
fromJSAddress j =
  { countryCode: j.countryCode
  , zipCode: toMaybe j.zipCode
  , city: toMaybe j.city
  , streetAddress: j.streetAddress
  }
