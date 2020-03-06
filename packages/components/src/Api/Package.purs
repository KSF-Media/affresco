module KSF.Api.Package where

import Prelude

import Data.Array (find)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)

type Package =
  { id           :: String
  , name         :: String
  , paper        :: { code :: String, name :: String }
  , products     :: Array Product
  , offers       :: Array PackageOffer
  , campaigns    :: Array Campaign
  , nextDelivery :: Nullable JSDate
  , description  :: Nullable PackageDescription
  , digitalOnly  :: Boolean
  }

type PackageDescription =
  { brand     :: String
  , brandLong :: String
  , descShort :: String
  , descLong  :: String
  , url       :: String
  , days      :: String
  , weekdays  :: String
  , frequency :: { amount :: Int, unit :: String }
  , includes  :: Array String
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

type Campaign =
  { no   :: Int
  , id   :: String
  , name :: String
  }

-- The packages we know how to render properly
data PackageName = HblPremium

derive instance genericPackageName :: Generic PackageName _
instance showPackageName :: Show PackageName where
  show = genericShow

toPackageId :: PackageName -> String
toPackageId HblPremium = "HBL WEvBB"

findPackage :: PackageName -> Array Package -> Either PackageValidationError Package
findPackage packageName packages =
  case find (\p -> p.id == toPackageId packageName) packages of
    Just p  -> Right p
    Nothing -> Left $ PackageNotFound packageName

data PackageValidationError
  = PackageOffersMissing PackageName
  | PackageNotFound PackageName

validatePackage :: PackageName -> Package -> Either PackageValidationError Package
validatePackage packageName p =
  -- See that there are offers found
  if Array.null p.offers
  then Left $ PackageOffersMissing packageName
  else Right p
