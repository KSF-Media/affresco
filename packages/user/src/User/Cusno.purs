module KSF.User.Cusno where

import Prelude

newtype Cusno = Cusno Int

instance eqCusno :: Eq Cusno where
  eq (Cusno c1) (Cusno c2) = c1 == c2

toString :: Cusno -> String
toString (Cusno c) = show c
