-- | This module is a small PureScript wrapper around the react-date-picker package.
--   The original react-date-picker docs can be found in https://www.npmjs.com/package/react-date-picker
module DatePicker.Component where

import Prelude

import Data.Date (Date)
import Data.DateTime (adjust)
import Data.DateTime as DateTime
import Data.DateTime.Instant as Instant
import Data.Function.Uncurried (Fn0, runFn0)
import Data.JSDate (JSDate, toDateTime, fromInstant)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Time.Duration (Minutes(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Math (abs)
import Prim.Row (class Nub, class Union)
import React.Basic (JSX, ReactComponent)
import React.Basic as React
import React.Basic.DOM as DOM
import Record as Record

foreign import datePicker_ :: Fn0 (ReactComponent DatePickerProps)

type Props =
  ( onChange               :: Effect (Maybe Date) -> Effect Unit
  , className              :: String
  , value                  :: Maybe Date
  , format                 :: String
  , required               :: Boolean
  , minDate                :: Maybe Date
  , maxDate                :: Maybe Date
  , disabled               :: Boolean
  , locale                 :: String
  , defaultActiveStartDate :: Maybe Date
  )

type DefaultProps =
  ( defaultActiveStartDate :: Maybe Date
  )

type DatePickerProps =
  { onChange               :: EffectFn1 (Nullable JSDate) Unit
  , className              :: String
  , value                  :: Nullable JSDate
  , format                 :: String
  , required               :: Boolean
  , minDate                :: Nullable JSDate
  , maxDate                :: Nullable JSDate
  , disabled               :: Boolean
  , locale                 :: String
  , defaultActiveStartDate :: Nullable JSDate
  }

datePicker :: forall attrs attrs_ . Union attrs DefaultProps attrs_ => Nub attrs_ Props => Record attrs -> JSX
datePicker userProps =
  DOM.div
    { className: "date-picker--wrapper"
    , children: [ picker ]
    }
  where
    props = Record.merge userProps defaultProps
    defaultProps :: Record DefaultProps
    defaultProps =
      { defaultActiveStartDate: Nothing
      }
    picker = React.element (runFn0 datePicker_) datePickerProps
    datePickerProps :: DatePickerProps
    datePickerProps =
      { className: props.className
      , value:     toNullable $ (fromInstant <<< Instant.fromDate) <$> props.value
      , format:    props.format
      , required:  props.required
      , minDate:   toNullable $ (fromInstant <<< Instant.fromDate) <$> props.minDate
      , maxDate:   toNullable $ (fromInstant <<< Instant.fromDate) <$> props.maxDate
      , disabled:  props.disabled
      , locale:    props.locale
      , onChange:  mkEffectFn1 $ adjustTimezone >>> props.onChange
      , defaultActiveStartDate: toNullable $ (fromInstant <<< Instant.fromDate) <$> props.defaultActiveStartDate
      }

-- | Glues current timezone to the JS date we get here.
--   Context:
--   The react-date-picker calls `Date​.prototype​.get​Time()` when processing the dates
--   in the calendar, and `Date​.prototype​.get​Time()` uses UTC for time representation.
--   Apparently this is not a bug, but a feature, of the react-date-picker:
--   https://github.com/wojtekmaj/react-calendar/issues/174#issuecomment-472108379
adjustTimezone :: Nullable JSDate -> Effect (Maybe Date)
adjustTimezone date
  | Just pickedDate <- toMaybe date
  , Just pickedDateTime <- toDateTime pickedDate
  = do
    offset <- JSDate.getTimezoneOffset pickedDate
    let offsetMinutes = Minutes (abs offset)
    pure $ DateTime.date <$> adjust offsetMinutes pickedDateTime
  | otherwise = pure Nothing
