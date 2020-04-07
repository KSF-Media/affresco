module Vetrina.Types where

import KSF.User as User

data AccountStatus
  = NewAccount
  | ExistingAccount
  | LoggedInAccount User.User
