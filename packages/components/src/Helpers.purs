module KSF.Helpers where

import           Prelude

import           Control.Alt ((<|>))
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
import           Data.Maybe              (Maybe (..), fromJust)
import           Data.String             (Pattern (..))
import           Data.String             as String
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

-- "2021-04-29T08:45:00Z" RFC3339 format
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

-- Local time aware formatter for RFC3339.  Won't apply any offset by
-- its own.  Probably only useful for output.
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
      offsetStr o = show (div o 60) <> ":" <>
                    let r = rem o 60 in if r < 10 then "0" <> show r else show r

-- RFC3339 parser.  Returns offset but doesn't apply it.
parseLocalDateTime :: String -> Maybe { offset :: Int, dateTime :: DateTime }
parseLocalDateTime str = do
  -- time-offset starts always at pos 19 with RFC3339
  let { after, before } = String.splitAt 19 str
  offset <- case String.splitAt 1 after of
    {before: "Z"} -> pure 0
    {before: "+", after: offsetStr } -> parseOffset offsetStr
    {before: "-", after: offsetStr } -> negate <$> parseOffset offsetStr
    _ -> Nothing
  dateTime <- hush $ unformat dateTimeFormatter $ before <> "Z"
  pure { offset, dateTime }
  where
    parseOffset x = do
      idx <- String.lastIndexOf (Pattern ":") x
      hours <- Int.fromString $ String.take idx x
      minutes <- Int.fromString $ String.drop (idx + 1) x
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
