module Vetrina.Types where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import KSF.User as User
import React.Basic (JSX)

data AccountStatus
  = NewAccount
  | ExistingAccount String
  | LoggedInAccount User.User

type Product =
  { id                           :: String
  , name                         :: String
  , description                  :: JSX
  , descriptionPurchaseCompleted :: JSX
  , priceCents                   :: Int
  , campaignNo                   :: Maybe Int
  }

type JSProduct =
  { id                           :: Nullable String
  , name                         :: Nullable String
  , description                  :: Nullable JSX
  , descriptionPurchaseCompleted :: Nullable JSX
  , priceCents                   :: Nullable Int
  , campaignNo                   :: Nullable Int
  }

fromJSProduct :: JSProduct -> Maybe Product
fromJSProduct jsProduct = do
  id          <- toMaybe jsProduct.id
  name        <- toMaybe jsProduct.name
  description <- toMaybe jsProduct.description
  priceCents  <- toMaybe jsProduct.priceCents
  let campaignNo = toMaybe jsProduct.campaignNo
      descriptionPurchaseCompleted = fromMaybe mempty $ toMaybe jsProduct.descriptionPurchaseCompleted
  pure { id, name, description, priceCents, campaignNo, descriptionPurchaseCompleted }
