module KSF.Api.Package where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Either (Either)
import Data.JSDate (JSDate)
import Data.DateTime (DateTime)
import Data.Nullable (Nullable, toMaybe)
import KSF.Helpers (parseDateTime)
import Data.Maybe (Maybe)
import Data.String as String
import Data.Newtype (class Newtype, un, unwrap)
import Data.Tuple (Tuple(..))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import OpenApiClient (OpenApiDate)

type PackageId = String

newtype Package = Package
  { id           :: PackageId
  , name         :: String
  , paper        :: { code :: String, name :: String }
  , products     :: Array Product
  , offers       :: Array PackageOffer
  , campaigns    :: Array Campaign
  , nextDelivery :: Maybe DateTime
  , digitalOnly  :: Boolean
  , canPause     :: Boolean
  , canTempAddr  :: Boolean
  , info         :: Array String
  }

derive instance newtypePackage :: Newtype Package _

instance decodeJsonPackage :: DecodeJson Package where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    name <- obj .: "name"
    paper <- obj .: "paper"
    products <- obj .: "products"
    offers <- obj .: "offers"
    campaigns <- obj .: "campaigns"
    nextDelivery <- map (map unwrap) (obj .:? "nextDelivery" :: Either JsonDecodeError (Maybe OpenApiDate))
    digitalOnly <- obj .: "digitalOnly"
    canPause <- obj .: "canPause"
    canTempAddr <- obj .: "canTempAddr"
    info <- obj .: "info"
    pure $ Package { id, name, paper, products, offers, campaigns, nextDelivery, digitalOnly, canPause, canTempAddr, info }


type PackageOffer =
  { months       :: Int
  , totalPrice   :: Int
  , monthlyPrice :: Int
  }

newtype Product = Product
  { id           :: String
  , name         :: String
  , active       :: ActiveDays
  , nextDelivery :: Maybe DateTime
  }

derive instance newtypeProduct :: Newtype Product _

instance decodeJsonProduct :: DecodeJson Product where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    name <- obj .: "name"
    active <- obj .: "active"
    nextDelivery <-  map (map unwrap) (obj .:? "nextDelivery" :: Either JsonDecodeError (Maybe OpenApiDate))
    pure $ Product { id, name, active, nextDelivery }

type ActiveDays =
  { mon :: Boolean
  , tue :: Boolean
  , wed :: Boolean
  , thu :: Boolean
  , fri :: Boolean
  , sat :: Boolean
  , sun :: Boolean
  }

data CampaignLengthUnit
  = Day
  | Week
  | Month
  | Year

instance showCampaignLengthUnit :: Show CampaignLengthUnit where
  show Day = "day"
  show Week = "week"
  show Month = "month"
  show Year = "year"

instance decodeJsonCampaignLengthUnit :: DecodeJson CampaignLengthUnit where
  decodeJson = map toCampaignLengthUnit <<< decodeJson

toCampaignLengthUnit :: String -> CampaignLengthUnit
toCampaignLengthUnit lengthUnit =
  case String.toLower lengthUnit of
    "day"   -> Day
    "week"  -> Week
    "month" -> Month
    "year"  -> Year
    -- Default to Month if not defined,
    -- as this is the most common case
    _       -> Month

toSwedish :: CampaignLengthUnit -> Tuple String String
toSwedish lengthUnit =
  case lengthUnit of
    Day   -> Tuple "dag"   "dagar"
    Week  -> Tuple "vecka" "veckor"
    Month -> Tuple "månad" "månader"
    Year  -> Tuple "år"    "år"

type Campaign =
  { no         :: Int
  , id         :: String
  , name       :: String
  , length     :: Int
  , lengthUnit :: CampaignLengthUnit
  , priceEur   :: Number
  }

type JSCampaign =
  { no         :: Nullable Int
  , id         :: Nullable String
  , name       :: Nullable String
  , length     :: Nullable Int
  , lengthUnit :: Nullable String
  , priceEur   :: Nullable Number
  }

fromJSCampaign :: JSCampaign -> Maybe Campaign
fromJSCampaign jsCampaign =
  { id: _
  , no: _
  , name: _
  , length: _
  , lengthUnit: _
  , priceEur: _
  }
  <$> toMaybe jsCampaign.id
  <*> toMaybe jsCampaign.no
  <*> toMaybe jsCampaign.name
  <*> toMaybe jsCampaign.length
  <*> (map toCampaignLengthUnit $ toMaybe jsCampaign.lengthUnit)
  <*> toMaybe jsCampaign.priceEur
