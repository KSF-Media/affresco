module KSF.Api.Package where

import Data.JSDate (JSDate)
import Data.Nullable (Nullable)

type PackageId = String

type Package =
  { id           :: PackageId
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
