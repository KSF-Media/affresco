module KSF.TemporaryAddressChange.Types where

import Data.Maybe (Maybe)

type AddressChange =
  { streetAddress :: Maybe String
  , zipCode       :: Maybe String
  , cityName      :: Maybe String
  , countryCode   :: Maybe String
  , temporaryName :: Maybe String
  }
