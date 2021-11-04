module KSF.Paper where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String

data Paper = HBL | ON | VN | KSF | JUNIOR
derive instance eqPaper :: Eq Paper
derive instance ordPaper :: Ord Paper

fromString :: String -> Maybe Paper
fromString paperString =
  case String.toUpper paperString of
    "HBL"    -> Just HBL
    "ON"     -> Just ON
    "ÖN"     -> Just ON
    "ÖNY"    -> Just ON
    "VN"     -> Just VN
    "KSF"    -> Just KSF
    "JUNIOR" -> Just JUNIOR
    _        -> Nothing

toString :: Paper -> String
toString HBL    = "HBL"
toString VN     = "VN"
toString ON     = "ON"
toString KSF    = "KSF"
toString JUNIOR = "JUNIOR"

paperName :: Paper -> String
paperName HBL    = "Hufvudstadtsbladet"
paperName ON     = "Östnyland"
paperName VN     = "Västra Nyland"
paperName JUNIOR = "HBL Junior"
paperName KSF    = "KSF Media"
