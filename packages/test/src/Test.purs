module KSF.Test where

import Prelude

import Data.DateTime (DateTime)
import Data.Formatter.DateTime as Format
import Data.List as List
import Effect (Effect)
import Effect.Now as Now

getTimeStamp :: Effect String
getTimeStamp = formatDate <$> Now.nowDateTime

formatDate :: DateTime -> String
formatDate = Format.format $ List.fromFoldable
  [ Format.YearFull
  , Format.MonthTwoDigits
  , Format.DayOfMonthTwoDigits
  , Format.Hours24
  , Format.MinutesTwoDigits
  , Format.SecondsTwoDigits
  ]
