module Vetrina.Types where

import KSF.User as User

data AccountStatus
  = NewAccount
  | ExistingAccount String
  | LoggedInAccount User.User

type Product =
  { id          :: String
  , description :: Array String
  , priceCents  :: Int
  }
