module KSF.Paper where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Read (class Read, read)

data Paper = HBL | ON | VN | KSF
derive instance eqPaper :: Eq Paper
instance readPaper :: Read Paper where
  read p =
    case String.toUpper p of
    "HBL" -> Just HBL
    "ON"  -> Just ON
    "ÖN"  -> Just ON
    "ÖNY" -> Just ON
    "VN"  -> Just VN
    "KSF" -> Just KSF
    _     -> Nothing

fromString :: String -> Maybe Paper
fromString paperString =
  case String.toUpper paperString of
    "HBL" -> Just HBL
    "ON"  -> Just ON
    "ÖN"  -> Just ON
    "ÖNY" -> Just ON
    "VN"  -> Just VN
    "KSF" -> Just KSF
    _     -> Nothing

toString :: Paper -> String
toString HBL = "HBL"
toString VN  = "VN"
toString ON  = "ON"
toString KSF = "KSF"
