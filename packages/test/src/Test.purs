module KSF.Test where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Formatter.DateTime as Format
import Data.List as List
import Effect (Effect)
import Effect.Now as Now
import KSF.Helpers as Helpers

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

-- For input to date component
formatDateSolid :: Date -> String
formatDateSolid date = Format.format formatter $ DateTime date Helpers.midnight
  where
    formatter = List.fromFoldable
      [ Format.DayOfMonthTwoDigits
      , Format.MonthTwoDigits
      , Format.YearFull
      ]
