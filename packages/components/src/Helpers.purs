module KSF.Helpers where

import           Prelude

import           Data.Date               (Date)
import           Data.DateTime           (DateTime (..), adjust)
import           Data.Enum               (toEnum)
import           Data.Formatter.DateTime (Formatter, FormatterCommand (..),
                                          format)
import           Data.Int                (toNumber)
import           Data.JSDate             as JSDate
import           Data.List               (fromFoldable)
import           Data.Maybe              (fromJust)
import           Data.String             (Pattern (..))
import           Data.String             as String
import           Data.Time               (Time (..))
import           Data.Time.Duration      (Minutes (..))
import           Data.Time.Duration      as Duration
import           Partial.Unsafe          (unsafePartial)

midnight :: Time
midnight = unsafePartial $ fromJust $ Time <$> toEnum 0 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0

formatDate :: Date -> String
formatDate date = format formatter $ DateTime date midnight
  where
    dash = Placeholder "-"
    formatter = fromFoldable
      [ YearFull
      , dash
      , MonthTwoDigits
      , dash
      , DayOfMonthTwoDigits
      ]

formatDateDots :: Date -> String
formatDateDots date = format formatter $ DateTime date midnight
  where
    dot = Placeholder "."
    formatter = fromFoldable
      [ DayOfMonthTwoDigits
      , dot
      , MonthTwoDigits
      , dot
      , YearFull
      ]

-- "2021-04-29T08:45:00Z"
dateTimeFormatter :: Formatter
dateTimeFormatter =
  fromFoldable
    [ YearFull
    , Placeholder "-"
    , MonthTwoDigits
    , Placeholder "-"
    , DayOfMonth
    , Placeholder "T"
    , Hours24
    , Placeholder ":"
    , MinutesTwoDigits
    , Placeholder ":"
    , SecondsTwoDigits
    , Placeholder "Z"
    ]

formatArticleTime :: DateTime -> String
formatArticleTime = format formatter
  where
    formatter =
      fromFoldable
      [ DayOfMonth
      , Placeholder "."
      , MonthTwoDigits
      , Placeholder "."
      , YearFull
      , Placeholder " "
      , Hours24
      , Placeholder ":"
      , MinutesTwoDigits
      ]

formatEur :: Int -> String
formatEur amountCent =
  let eurString = show $ toNumber amountCent / 100.0
  in case String.split (Pattern ".") eurString of
    [euros, cents] -> euros <> "." <> (String.take 2 $ cents <> "0")
    _              -> eurString
