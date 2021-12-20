module KSF.User.Cusno where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)

newtype Cusno = Cusno Int

instance eqCusno :: Eq Cusno where
  eq (Cusno c1) (Cusno c2) = c1 == c2

instance decodeJsonCusno :: DecodeJson Cusno where
  decodeJson = map Cusno <<< decodeJson

toString :: Cusno -> String
toString (Cusno c) = show c

fromString :: String -> Maybe Cusno
fromString = map Cusno <<< Int.fromString
