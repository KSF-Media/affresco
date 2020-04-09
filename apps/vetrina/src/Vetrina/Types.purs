module Vetrina.Types where

import Data.Maybe (Maybe)
import KSF.Api.Package as Package
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
  , packageName :: Package.PackageName
  , imageUrl    :: Maybe String -- TODO: What to do with this?
  }
