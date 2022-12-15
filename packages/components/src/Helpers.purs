module KSF.Helpers where

import           Prelude

import           Control.Alt ((<|>))
import           Data.Array              as Array
import           Data.Date               (Date, adjust)
import           Data.DateTime           (DateTime (..))
import           Data.DateTime           as DateTime
import           Data.Either             (hush)
import           Data.Enum               (toEnum)
import           Data.Formatter.DateTime (Formatter, FormatterCommand (..),
                                          format, unformat)
import           Data.Int                as Int
import           Data.Int                (toNumber, rem)
import           Data.List               (fromFoldable)
import           Data.Maybe              (Maybe (..), fromJust, fromMaybe)
import           Data.String             (Pattern (..), Replacement (..))
import           Data.String             as String
import           Data.String.Regex       as Regex
import           Data.Time               (Time (..))
import           Data.Time.Duration      (Days (..), Hours (..), negateDuration)
import           Effect                  (Effect)
import           Effect.Now              as Now
import           Partial.Unsafe          (unsafePartial)

foreign import getCurrentTZOffset_ :: Effect Int

midnight :: Time
midnight = unsafePartial $ fromJust $ Time <$> toEnum 0 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0

noon :: Time
noon = unsafePartial $ fromJust $ Time <$> toEnum 12 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0

getCurrentTZOffset :: Effect Hours
getCurrentTZOffset = Hours <<< toNumber <$> getCurrentTZOffset_

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

-- "2021-04-29T08:45:00Z" ISO 8601 format
dateTimeFormatter :: Formatter
dateTimeFormatter =
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
    , Placeholder "Z"
    ]

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
      offsetStr o = padLeadingZero (div o 60) <> ":" <> padLeadingZero (rem 0 60)

-- ISO 8601 parser.  Returns offset but doesn't apply it.
parseLocalDateTime :: String -> Maybe { offset :: Int, dateTime :: DateTime }
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
  pure { offset, dateTime }
  where
    -- Possible offset formats: HH, HHMM, HH:MM
    parseOffset x = do
      let bare = String.replace (Pattern ":") (Replacement "") x
      --idx <- String.lastIndexOf (Pattern ":") x
      hours <- Int.fromString $ String.take 2 bare
      let minutes = fromMaybe 0 $ Int.fromString $ String.drop 2 x
      pure $ 60 * hours + minutes

formatArticleTime :: DateTime -> String
formatArticleTime = format articleFormatter
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

formatEur :: Int -> String
formatEur amountCent =
  let eurString = show $ toNumber amountCent / 100.0
  in case String.split (Pattern ".") eurString of
    [euros, cents] -> euros <> "," <> (String.take 2 $ cents <> "0")
    _              -> eurString

paperInvoiceCents :: Int
paperInvoiceCents = 500

-- If doing temporary address change or pause before 12:00, allow it
-- to start from next day.  Otherwise, 2 days from now.
getMinStartDate :: Maybe Date -> Effect (Maybe Date)
getMinStartDate nextDelivery = do
  offset <- negateDuration <$> getCurrentTZOffset
  localNow <- DateTime.adjust offset <$> Now.nowDateTime
  case localNow of
    -- Shouldn't happen
    Nothing -> pure Nothing
    Just now -> do
      let soonestDuration = Days $ if DateTime.time now < noon then 1.0 else 2.0
          soonestStart = adjust soonestDuration (DateTime.date now)
          byNextIssue = max <$> soonestStart <*> nextDelivery
      pure $ byNextIssue <|> soonestStart
