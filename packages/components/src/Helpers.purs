module KSF.Helpers where

import           Prelude

import           Data.DateTime           (DateTime)
import           Data.Formatter.DateTime (FormatterCommand (..), format)
import           Data.Int                (toNumber)
import           Data.List               (fromFoldable)
import           Data.String             (Pattern (..))
import           Data.String             as String

formatDate :: DateTime -> String
formatDate = format formatter
  where
    dash = Placeholder "-"
    formatter = fromFoldable
      [ YearFull
      , dash
      , MonthTwoDigits
      , dash
      , DayOfMonthTwoDigits
      ]

formatEur :: Int -> String
formatEur amountCent =
  let eurString = show $ toNumber amountCent / 100.0
  in case String.split (Pattern ".") eurString of
    [euros, cents] -> euros <> "." <> (String.take 2 $ cents <> "0")
    _              -> eurString
