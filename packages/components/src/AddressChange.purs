module KSF.AddressChange where

import Data.Maybe (Maybe)

-- TODO WIP: move to Api
type AddressChange =
  { streetAddress :: Maybe String
  , zipCode       :: Maybe String
  , countryCode   :: Maybe String
  , temporaryName :: Maybe String
  }
