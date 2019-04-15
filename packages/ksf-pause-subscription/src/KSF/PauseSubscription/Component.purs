module KSF.PauseSubscription.Component where

import Prelude

import Data.DateTime (DateTime, adjust)
import Data.Function.Uncurried (Fn0, runFn0)
import Data.JSDate (JSDate, fromDateTime, toDateTime)
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Time.Duration as Time.Duration
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import KSF.Grid as Grid
import React.Basic (JSX, ReactComponent, StateUpdate(..), element, make, runUpdate)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended
import Unsafe.Coerce (unsafeCoerce)

foreign import pauseSubscriptionStyles :: Style
foreign import datePicker_ :: Fn0 (ReactComponent DatePickerProps)

type DatePickerProps =
  { onChange  :: EffectFn1 (Nullable JSDate) Unit
  , className :: String
  , value     :: Nullable JSDate
  , format    :: String
  , required  :: Boolean
  , minDate   :: Nullable JSDate
  , maxDate   :: Nullable JSDate
  , disabled  :: Boolean
  }

type Self = React.Self Props State

type Props =
  { subsno   :: Int
  , onCancel :: Effect Unit
  }

type State =
  { startDate :: Maybe DateTime
  , minStartDate :: Maybe DateTime
  , endDate :: Maybe DateTime
  , minEndDate :: Maybe DateTime
  , maxEndDate :: Maybe DateTime
  }

data Action
  = SetStartDate (Maybe DateTime)
  | SetMinStartDate (Maybe DateTime)
  | SetEndDate (Maybe DateTime)

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
  }

update :: Self -> Action -> StateUpdate Props State
update self action = Update $ case action of
  SetStartDate newStartDate ->
    self.state
      { startDate = newStartDate
      , minEndDate = calcMinEndDate newStartDate
      , maxEndDate = calcMaxEndDate newStartDate
      }
  SetEndDate newEndDate -> self.state { endDate = newEndDate }
  SetMinStartDate newMinStartDate -> self.state { minStartDate = newMinStartDate }

-- | Minimum pause period is one week
calcMinEndDate :: Maybe DateTime -> Maybe DateTime
calcMinEndDate Nothing = Nothing
calcMinEndDate (Just startDate) = do
  let week = Time.Duration.Days 7.0
  adjust week startDate

-- | Maximum pause period is three months
calcMaxEndDate :: Maybe DateTime -> Maybe DateTime
calcMaxEndDate Nothing = Nothing
calcMaxEndDate (Just startDate) = do
  let threeMonths = Time.Duration.Days (30.0 * 3.0)
  adjust threeMonths startDate

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  send self $ SetMinStartDate (Just now)

send :: Self -> Action -> Effect Unit
send = runUpdate update

render :: Self -> JSX
render self =
  React.Extended.requireStyle
    pauseSubscriptionStyles
    $ DOM.div
        { className: "clearfix pause-subscription--container"
        , children:
            [ Grid.row_
                [ DOM.div
                    { className: "col col-11"
                    , children: [ DOM.h3_ [ DOM.text "Uppehåll på prenumarationen" ] ]
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
          { onSubmit: handler preventDefault (\_ -> submitForm self.state.startDate self.state.endDate self.props.subsno)
          , children:
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
        "Börjar från"
        $ element
          datePicker
            { onChange: mkEffectFn1 \pickedDate -> do
                 send self $ SetStartDate $ toDateTime =<< toMaybe pickedDate
            , className: "pause-subscription--date-picker"
            , value: toNullable $ fromDateTime <$> self.state.startDate
            , format: "d.M.yyyy"
            , required: true
            , minDate: toNullable $ fromDateTime <$> self.state.minStartDate
            , maxDate: toNullable Nothing
            , disabled: false
            }

    endDayInput =
      dateInput
        "Skall starta igen"
        $ element
            datePicker
              { onChange: mkEffectFn1 \pickedDate -> do
                   send self $ SetEndDate $ toDateTime =<< toMaybe pickedDate
              , className: "pause-subscription--date-picker"
              , value: toNullable $ fromDateTime <$> self.state.endDate
              , format: "d.M.yyyy"
              , required: true
              , minDate: toNullable $ fromDateTime <$> self.state.minEndDate
              , maxDate: toNullable $ fromDateTime <$> self.state.maxEndDate
              , disabled: isNothing self.state.startDate
              }

    dateInput :: String -> JSX -> JSX
    dateInput labelText inputElem =
      Grid.row
        [ Grid.row_ [ DOM.label_ [ DOM.text labelText ] ]
        , Grid.row_ [ inputElem ]
        ]
        $ Just { extraClasses: [ "mt2" ] }

    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Skicka" ]
          , className: "button-green"
          }

submitForm :: Maybe DateTime -> Maybe DateTime -> Int -> Effect Unit
submitForm (Just startDay) (Just endDay) subsno =
  Console.log $ unsafeCoerce startDay
submitForm _ _ _ = pure unit

datePicker :: ReactComponent DatePickerProps
datePicker = runFn0 datePicker_
