module KSF.Api.Search where

import Data.Maybe           (Maybe)
import Data.UUID            (UUID)
import KSF.Api.Address      (Address)
import KSF.Api.Consent      (GdprConsent, LegalConsent)
import KSF.Api.Subscription (Subscription)
import KSF.User.Cusno       (Cusno)

type SearchQuery =
  { faroLimit   :: Int
  , query       :: String
  }

type JanrainUser =
  { uuid        :: UUID
  , email       :: Maybe String
  , firstName   :: Maybe String
  , lastName    :: Maybe String
  , consent     :: Array GdprConsent
  , legal       :: Array LegalConsent
  , cusno       :: Maybe String
  , otherCusnos :: Maybe (Array String)
  }

type FaroUser =
  { cusno       :: Cusno
  , name        :: String
  , address     :: Maybe Address
  , email       :: Maybe String
  , subs        :: Maybe (Array Subscription)
  }

type SearchResult =
  { janrain     :: Maybe JanrainUser
  , faro        :: Array FaroUser
  }
