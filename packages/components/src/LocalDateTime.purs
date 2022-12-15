module KSF.LocalDateTime where

import Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (hush)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String (Pattern(..), Replacement(..))
import Data.String.Regex as Regex
import KSF.Helpers (dateTimeFormatter)

data LocalDateTime = LocalDateTime Int DateTime
derive instance localDateTimeGeneric :: Generic LocalDateTime _
instance showLocalDateTime :: Show LocalDateTime where
  show = genericShow

-- Local time aware output formatter for ISO 8601.  Not really useful
-- for input since it uses a fixed offset.
localDateTimeFormatter :: Int -> Formatter
localDateTimeFormatter offset =
  fromFoldable
    [ YearFull
    , Placeholder "-"
    , MonthTwoDigits
    , Placeholder "-"
    , DayOfMonthTwoDigits
    , Placeholder "T"
    , Hours24
    , Placeholder ":"
    , MinutesTwoDigits
    , Placeholder ":"
    , SecondsTwoDigits
    , Placeholder $ if offset == 0 then "Z"
                    else if offset < 0
                         then "-" <> offsetStr (negate offset)
                         else "+" <> offsetStr offset
    ]
    where
      padLeadingZero n | n < 10 = "0" <> show n
      padLeadingZero n = show n
      offsetStr o = padLeadingZero (div o 60) <> ":" <> padLeadingZero (Int.rem 0 60)

-- Offset aware ISO 8601 parser.
parseLocalDateTime :: String -> Maybe LocalDateTime
parseLocalDateTime str = do
  offsetRegex <- hush $ Regex.regex "(.{19}(?:\\.[0-9]+)?)(Z|[+-].+)" mempty
  match <- Array.fromFoldable <$> Regex.match offsetRegex str
  after <- join $ Array.index match 2
  offset <- case String.splitAt 1 after of
    {before: "Z"} -> pure 0
    {before: "+", after: offsetStr } -> parseOffset offsetStr
    {before: "-", after: offsetStr } -> negate <$> parseOffset offsetStr
    _ -> Nothing
  -- Seconds precision is enough for our use
  dateTime <- hush $ unformat dateTimeFormatter $ (String.take 19 str) <> "Z"
  pure $ LocalDateTime offset dateTime
  where
    -- Possible offset formats: HH, HHMM, HH:MM
    parseOffset x = do
      let bare = String.replace (Pattern ":") (Replacement "") x
      --idx <- String.lastIndexOf (Pattern ":") x
      hours <- Int.fromString $ String.take 2 bare
      let minutes = fromMaybe 0 $ Int.fromString $ String.drop 2 x
      pure $ 60 * hours + minutes

-- ISO 8601 format with offset
formatLocalDateTime :: LocalDateTime -> String
formatLocalDateTime (LocalDateTime offset dateTime) =
  format (localDateTimeFormatter offset) dateTime

-- Human readable local date
formatArticleTime :: LocalDateTime -> String
formatArticleTime = format articleFormatter <<< (\(LocalDateTime _ dateTime) -> dateTime)
  where
   articleFormatter =
    fromFoldable
      [ DayOfMonthTwoDigits
      , Placeholder "."
      , MonthTwoDigits
      , Placeholder "."
      , YearFull
      , Placeholder " "
      , Hours24
      , Placeholder ":"
      , MinutesTwoDigits
      ]
