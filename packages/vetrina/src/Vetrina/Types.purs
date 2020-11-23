module Vetrina.Types where

import Prelude

import Data.Array (mapMaybe)
import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import KSF.User as User
import React.Basic (JSX)

data AccountStatus
  = NewAccount
  | ExistingAccount String
  | LoggedInAccount User.User

type JSProductContent =
  { title       :: Nullable String
  , description :: Nullable String
  }

type ProductContent =
  { title       :: String
  , description :: String
  }

type Product =
  { id                           :: String
  , name                         :: String
  , description                  :: JSX
  , descriptionPurchaseCompleted :: JSX
  , priceCents                   :: Int
  , campaignNo                   :: Maybe Int
  , contents                     :: Array ProductContent
  }

type JSProduct =
  { id                           :: Nullable String
  , name                         :: Nullable String
  , description                  :: Nullable JSX
  , descriptionPurchaseCompleted :: Nullable JSX
  , priceCents                   :: Nullable Int
  , campaignNo                   :: Nullable Int
  , contents                     :: Nullable (Array JSProductContent)
  }

fromJSProduct :: JSProduct -> Maybe Product
fromJSProduct jsProduct = do
  id          <- toMaybe jsProduct.id
  name        <- toMaybe jsProduct.name
  description <- toMaybe jsProduct.description
  priceCents  <- toMaybe jsProduct.priceCents
  let campaignNo = toMaybe jsProduct.campaignNo
      descriptionPurchaseCompleted = fold $ toMaybe jsProduct.descriptionPurchaseCompleted
      contents = mapMaybe fromJSProductContent $ fold $ toMaybe jsProduct.contents
  pure { id, name, description, priceCents, campaignNo, descriptionPurchaseCompleted, contents }

fromJSProductContent ::  JSProductContent -> Maybe ProductContent
fromJSProductContent jsProduct =
  { title: _
  , description: _
  }
  <$> toMaybe jsProduct.title
  <*> toMaybe jsProduct.description
