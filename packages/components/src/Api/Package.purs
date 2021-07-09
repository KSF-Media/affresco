module KSF.Api.Package where

import Prelude

import Data.Date (Date)
import Data.JSDate (JSDate, toDate)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

type PackageId = String

type Package =
  { id           :: PackageId
  , name         :: String
  , paper        :: { code :: String, name :: String }
  , products     :: Array Product
  , offers       :: Array PackageOffer
  , campaigns    :: Array Campaign
  , nextDelivery :: Maybe Date
  , digitalOnly  :: Boolean
  , canPause     :: Boolean
  , canTempAddr  :: Boolean
  , info         :: Array String
  }

type JSPackage =
  { id           :: PackageId
  , name         :: String
  , paper        :: { code :: String, name :: String }
  , products     :: Array JSProduct
  , offers       :: Array PackageOffer
  , campaigns    :: Array JSCampaign
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
  , nextDelivery :: Maybe Date
  }

type JSProduct =
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

fromJSPackage :: JSPackage -> Maybe Package
fromJSPackage jsPackage = do
  let fromJSProduct jsProduct =
        { id: jsProduct.id
        , name: jsProduct.name
        , active: jsProduct.active
        , nextDelivery: _
        }
        <$> (case toMaybe jsProduct.nextDelivery of
                Nothing -> Just Nothing
                Just x -> Just <$> toDate x)
  products <- sequence $ map fromJSProduct jsPackage.products
  campaigns <- sequence $ map fromJSCampaign jsPackage.campaigns
  nextDelivery <- case toMaybe jsPackage.nextDelivery of
    Nothing -> Just Nothing
    Just x -> Just <$> toDate x
  pure
    { id: jsPackage.id
    , name: jsPackage.name
    , paper: jsPackage.paper
    , products
    , offers: jsPackage.offers
    , campaigns
    , nextDelivery
    , digitalOnly: jsPackage.digitalOnly
    , canPause: jsPackage.canPause
    , canTempAddr: jsPackage.canTempAddr
    , info: jsPackage.info
    }
