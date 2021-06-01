module Vetrina.Types where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import KSF.Api.Package (Campaign, JSCampaign, fromJSCampaign)
import KSF.User as User
import React.Basic (JSX)

data AccountStatus
  = NewAccount
  | ExistingAccount String
  | LoggedInAccount User.User

instance eqAccountStatus :: Eq AccountStatus where
  eq NewAccount NewAccount = true
  eq (ExistingAccount _) (ExistingAccount _) = true
  eq (LoggedInAccount _) (LoggedInAccount _) = true
  eq _ _ = false

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
  , campaign                     :: Maybe Campaign
  , contents                     :: Array ProductContent
  }

type JSProduct =
  { id                           :: Nullable String
  , name                         :: Nullable String
  , description                  :: Nullable JSX
  , descriptionPurchaseCompleted :: Nullable JSX
  , priceCents                   :: Nullable Int
  , campaign                     :: Nullable JSCampaign
  , contents                     :: Nullable (Array JSProductContent)
  }

fromJSProduct :: JSProduct -> Maybe Product
fromJSProduct jsProduct = do
  id          <- toMaybe jsProduct.id
  name        <- toMaybe jsProduct.name
  priceCents  <- toMaybe jsProduct.priceCents
  let description = fold $ toMaybe jsProduct.description
  -- NOTE: Campaign validation needs to be done separately
  let campaign = fromJSCampaign =<< toMaybe jsProduct.campaign
  let descriptionPurchaseCompleted = fold $ toMaybe jsProduct.descriptionPurchaseCompleted
      contents = mapMaybe fromJSProductContent $ fold $ toMaybe jsProduct.contents
  pure { id, name, description, priceCents, campaign, descriptionPurchaseCompleted, contents }

fromJSProductContent ::  JSProductContent -> Maybe ProductContent
fromJSProductContent jsProduct =
  { title: _
  , description: _
  }
  <$> toMaybe jsProduct.title
  <*> toMaybe jsProduct.description

parseJSCampaign :: JSProduct -> Either String (Maybe Campaign)
parseJSCampaign jsProduct =
  case toMaybe jsProduct.campaign of
    Nothing -> Right Nothing
    Just jsCampaign ->
      case fromJSCampaign jsCampaign of
        Just validCampaign -> Right $ Just validCampaign
        Nothing            -> Left "Could not parse campaign"
