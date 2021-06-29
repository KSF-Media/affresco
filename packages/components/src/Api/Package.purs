module KSF.Api.Package where

import Prelude

import Data.JSDate (JSDate)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.String as String
import Data.Tuple (Tuple(..))

type PackageId = String

type Package =
  { id           :: PackageId
  , name         :: String
  , paper        :: { code :: String, name :: String }
  , products     :: Array Product
  , offers       :: Array PackageOffer
  , campaigns    :: Array Campaign
  , nextDelivery :: Nullable JSDate
  , digitalOnly  :: Boolean
  , canPause     :: Boolean
  , canTempAddr  :: Boolean
  , info         :: Array String
  }

type PackageOffer =
  { months       :: Int
  , totalPrice   :: Int
  , monthlyPrice :: Int
  }

type Product =
  { id           :: String
  , name         :: String
  , active       :: ActiveDays
  , nextDelivery :: Nullable JSDate
  }

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
