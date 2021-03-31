module KSF.PauseSubscription.Component where

import Prelude

import Data.Date (Date, adjust)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Time.Duration as Time.Duration
import Data.Tuple (Tuple(..))
import DatePicker.Component as DatePicker
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import KSF.Helpers (formatDateDots)
import KSF.Grid as Grid
import KSF.User as User
import KSF.User.Cusno (Cusno)
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import KSF.Tracking as Tracking

type Self = React.Self Props State

type Props =
  { subsno    :: Int
  , cusno     :: Cusno
  , userUuid  :: User.UUID
  , oldStart  :: Maybe Date
  , oldEnd    :: Maybe Date
  , onCancel  :: Effect Unit
  , onLoading :: Effect Unit
  , onSuccess :: User.Subscription -> Effect Unit
  , onError   :: User.InvalidDateInput -> Effect Unit
  }

type State =
  { startDate    :: Maybe Date
  , minStartDate :: Maybe Date
  , endDate      :: Maybe Date
  , minEndDate   :: Maybe Date
  , maxEndDate   :: Maybe Date
  , ongoing      :: Boolean
  }

pauseSubscription :: Props -> JSX
pauseSubscription = make component { initialState, render, didMount }

component :: React.Component Props
component = React.createComponent "PauseSubscription"

initialState :: State
initialState =
  { startDate: Nothing
  , minStartDate: Nothing
  , endDate: Nothing
  , minEndDate: Nothing
  , maxEndDate: Nothing
  , ongoing: false
  }

-- | Minimum pause period is one week
calcMinEndDate :: Maybe Date -> Maybe Date
calcMinEndDate Nothing = Nothing
calcMinEndDate (Just startDate) = do
  -- 6 days added to the starting date = 7 (one week)
  let week = Time.Duration.Days 6.0
  adjust week startDate

-- | Maximum pause period is three months
calcMaxEndDate :: Maybe Date -> Maybe Date
calcMaxEndDate Nothing = Nothing
calcMaxEndDate (Just startDate) = do
  let threeMonths = Time.Duration.Days (30.0 * 3.0)
  adjust threeMonths startDate

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDate
  -- We set the minimum start date two days ahead because of system issues.
  -- TODO: This could be set depending on the time of day
  let dayAfterTomorrow = adjust (Time.Duration.Days 2.0) now
      ongoing = case self.props.oldStart of
        Nothing -> false
        Just date -> date <= now
  self.setState _ { minStartDate = dayAfterTomorrow
                  , startDate = self.props.oldStart
                  , ongoing = ongoing
                  }

render :: Self -> JSX
render self =
  DOM.div
    { className: "clearfix pause-subscription--container"
    , children:
        [ Grid.row_
            [ DOM.div
                { className: "col col-11"
                , children: [ DOM.h3_ [ DOM.text "Uppehåll på prenumerationen" ] ]
                }
            , DOM.div
                { className: "col-1 flex pause-subscription--close-icon"
                , children: [ DOM.div { className: "close-icon" } ]
                , onClick: handler_ self.props.onCancel
                }
            ]
        , pauseForm
        ]
    }
  where
    pauseForm =
      DOM.form
          { onSubmit: handler preventDefault (\_ -> submitForm self.state self.props)
          , children:
              (case Tuple self.props.oldStart self.props.oldEnd of
                  Tuple (Just start) (Just end) ->
                    [ DOM.text $ "Ursprunglig: " <> formatDateDots start <> " – " <> formatDateDots end ]
                  _ -> []) <>
              [ startDayInput
              , endDayInput
              , DOM.div
                  { children: [ submitFormButton ]
                  , className: "mt2 clearfix"
                  }
              ]
          }

    startDayInput =
      dateInput
        self
        { action: onStartDateChange
        , value: if self.state.ongoing then self.props.oldStart else self.state.startDate
        , minDate: if self.state.ongoing then self.props.oldStart else self.state.minStartDate
        , maxDate: Nothing
        , disabled: self.state.ongoing
        , label: "Börjar från"
        }

    endDayInput =
      dateInput
        self
        { action: \newEndDate -> self.setState _ { endDate = newEndDate }
        , value: self.state.endDate
        , minDate: self.state.minEndDate
        , maxDate: self.state.maxEndDate
        , disabled: isNothing self.state.startDate
        , label: "Avslutas"
        }

    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Skicka" ]
          , className: "button-green"
          }

    onStartDateChange newStartDate =
      self.setState _
        { startDate = newStartDate
        , minEndDate = calcMinEndDate newStartDate
        , maxEndDate = calcMaxEndDate newStartDate
        }

type DateInputField =
  { action   :: Maybe Date -> Effect Unit
  , value    :: Maybe Date
  , minDate  :: Maybe Date
  , maxDate  :: Maybe Date
  , disabled :: Boolean
  , label    :: String
  }

dateInput :: Self -> DateInputField -> JSX
dateInput self { action, value, minDate, maxDate, disabled, label } =
  Grid.row
    [ Grid.row_ [ DOM.label_ [ DOM.text label ] ]
    , Grid.row_
        [ DatePicker.datePicker
            { onChange: (action =<< _)
            , className: "pause-subscription--date-picker"
            , value: value
            , format: "d.M.yyyy"
            , required: true
            , minDate: minDate
            , maxDate: maxDate
            , disabled
            , locale: "sv-FI"
            }
        ]
    ]
    $ Just { extraClasses: [ "mt2" ] }

submitForm :: State -> Props -> Effect Unit

submitForm { startDate: Just start, endDate: Just end, ongoing } props@{ userUuid, subsno, oldStart: Just oldStart, oldEnd: Just oldEnd} = do
  props.onLoading
  let newStart = if ongoing then oldStart else start
  Aff.launchAff_ $
    User.editSubscriptionPause userUuid subsno oldStart oldEnd newStart end >>=
      case _ of
        Right sub -> liftEffect do
          props.onSuccess sub
          Tracking.editSubscriptionPause props.cusno (show subsno) oldStart oldEnd start end "success"
        Left invalidDateInput -> liftEffect do
          props.onError invalidDateInput
          Tracking.editSubscriptionPause props.cusno (show subsno) oldStart oldEnd start end "error: invalid date input"

submitForm { startDate: Just start, endDate: Just end } props@{ userUuid, subsno } = do
  props.onLoading
  Aff.launchAff_ $
    User.pauseSubscription userUuid subsno start end >>=
      case _ of
        Right sub -> liftEffect do
          props.onSuccess sub
          Tracking.pauseSubscription props.cusno (show subsno) start end "success"
        Left invalidDateInput -> liftEffect do
          props.onError invalidDateInput
          Tracking.pauseSubscription props.cusno (show subsno) start end "error: invalid date input"

submitForm _ _ = Console.error "Pause subscription dates were not defined."
