module HBL365.Product where

import Prelude

import Bottega (getPackages)
import Control.Monad.Error.Class (try)
import Data.Array (head)
import Data.Either (Either)
import Data.Foldable (find)
import Data.Maybe (maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, throwException)
import Vetrina.Types (Product)

getProduct :: Aff (Either Error Product)
getProduct = try do
  packages <- getPackages
  maybe (liftEffect $ throwException $ error "Could not find product from packages") pure do
    hbl365 <- find (\x -> x.id == "HBL 365") packages
    priceCents <- _.monthlyPrice <$> head hbl365.offers
    pure
      { id: "HBL 365"
      , name: hbl365.name
      , description: mempty
      , descriptionPurchaseCompleted: mempty
      , priceCents
      , campaign: find (\x -> x.id == "1M1E") hbl365.campaigns
      , contents: []
      }
