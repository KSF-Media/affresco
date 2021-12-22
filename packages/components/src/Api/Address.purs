module KSF.Api.Address where

import           Data.Maybe (Maybe)

type Address =
  { countryCode   :: String
  , zipCode       :: Maybe String
  , city          :: Maybe String
  , streetAddress :: String
  , streetName    :: Maybe String
  , houseNo       :: Maybe String
  , staircase     :: Maybe String
  , apartment     :: Maybe String
  }
