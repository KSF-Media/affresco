module Vetrina.Types where

import Data.Maybe (Maybe)
import KSF.User as User

data AccountStatus
  = NewAccount
  | ExistingAccount String
  | LoggedInAccount User.User

type Product =
  { name        :: String
  , id          :: String
  , description :: Array String
  , priceCents  :: Int
  , imageUrl    :: Maybe String -- TODO: What to do with this?
  }
