module KSF.Ranges where

import Prelude

import Data.Array (catMaybes)
import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..))
import Data.Foldable (any, minimum)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Time.Duration (Days(..), negateDuration)

type Range =
  { rangeMin :: DateTime
  , rangeMax :: Maybe DateTime
  }

type MinMaxRanges =
  { minSpan :: Days
  , maxSpan :: Maybe Days
  , spanBetween :: Days
  , ranges :: Array Range
  }

type Result =
  { minEndDate :: Maybe DateTime
  , maxEndDate :: Maybe DateTime
  }

calcMinEndDate :: MinMaxRanges -> DateTime -> Either Unit (Maybe DateTime)
calcMinEndDate { minSpan, spanBetween, ranges } startDate =
  -- Check that there's room for one minSpan+spanBetween before an existing range
  let adjustedStart = fromMaybe startDate $
                      adjust minSpan =<< adjust spanBetween startDate
      withinRange {rangeMin, rangeMax} =
        let adjustedMin = fromMaybe rangeMin $ adjust (negateDuration spanBetween) rangeMin
        in rangeMin <= adjustedStart && maybe true (startDate <= _) rangeMax
  in if any withinRange ranges
       then Left unit
       else pure $ adjust minSpan startDate

calcMaxEndDate :: MinMaxRanges -> DateTime -> Maybe DateTime
calcMaxEndDate { minSpan, maxSpan, spanBetween, ranges } startDate =
  let f { rangeMin } = do
        _ <- if rangeMin < startDate then Nothing else pure unit
        adjust (Days (-1.0)) =<< adjust (negateDuration spanBetween) rangeMin
      rangesWithin = catMaybes $ f <$> ranges
  in minimum $ rangesWithin <> (maybe [] pure $ flip adjust startDate =<< maxSpan)

calcEndDates :: MinMaxRanges -> Maybe DateTime -> Maybe Result
calcEndDates ranges startDate =
  case startDate of
    Nothing -> Just { minEndDate: Nothing, maxEndDate: Nothing }
    Just date -> case calcMinEndDate ranges date of
      (Left _) -> Nothing
      (Right minEndDate) -> Just { minEndDate, maxEndDate: calcMaxEndDate ranges date }
