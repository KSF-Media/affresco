module Vetrina.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import KSF.User as User

data AccountStatus
  = NewAccount
  | ExistingAccount String
  | LoggedInAccount User.User

type Product =
  { id          :: String
  , description :: Array String
  , priceCents  :: Int
  , campaignNo  :: Maybe Int
  }

type JSProduct =
  { id          :: Nullable String
  , description :: Nullable (Array String)
  , priceCents  :: Nullable Int
  , campaignNo  :: Nullable Int
  }

fromJSProduct :: JSProduct -> Maybe Product
fromJSProduct jsProduct = do
  id          <- toMaybe jsProduct.id
  description <- toMaybe jsProduct.description
  priceCents  <- toMaybe jsProduct.priceCents
  let campaignNo = toMaybe jsProduct.campaignNo
  pure { id, description, priceCents, campaignNo }
