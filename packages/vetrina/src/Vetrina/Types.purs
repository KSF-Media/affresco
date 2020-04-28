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
  , price       :: Number
  , imageUrl    :: Maybe String -- TODO: What to do with this?
  }
