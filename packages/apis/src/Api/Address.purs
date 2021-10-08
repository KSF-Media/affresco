module KSF.Api.Address where

import Data.Nullable (Nullable)

type Address =
  { countryCode   :: String
  , zipCode       :: Nullable String
  , city          :: Nullable String
  , streetAddress :: String
  , streetName    :: Nullable String
  , houseNo       :: Nullable String
  , staircase     :: Nullable String
  , apartment     :: Nullable String
  }
