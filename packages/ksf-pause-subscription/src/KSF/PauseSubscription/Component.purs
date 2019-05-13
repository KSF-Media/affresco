module KSF.PauseSubscription.Component where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.DateTime (DateTime, adjust)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended

foreign import pauseSubscriptionStyles :: Style

type Self = React.Self Props State

type Props =
  { subsno    :: Int
  , userUuid  :: Persona.UUID
  , onCancel  :: Effect Unit
  , onLoading :: Effect Unit
  , onSuccess :: Persona.Subscription -> Effect Unit
  , onError   :: Persona.InvalidPauseDateError -> Effect Unit
  }

type State =
  { startDate    :: Maybe DateTime
  , minStartDate :: Maybe DateTime
  , endDate      :: Maybe DateTime
  , minEndDate   :: Maybe DateTime
  , maxEndDate   :: Maybe DateTime
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
  let tomorrow = adjust (Time.Duration.Days 1.0) now
  send self $ SetMinStartDate tomorrow

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
        { action: SetStartDate
        , value: self.state.startDate
        , minDate: self.state.minStartDate
        , maxDate: Nothing
        , disabled: false
        , label: "Börjar från"
        }

    endDayInput =
      dateInput
        self
        { action: SetEndDate
        , value: self.state.endDate
        , minDate: self.state.minEndDate
        , maxDate: self.state.maxEndDate
        , disabled: isNothing self.state.startDate
        , label: "Skall starta igen"
        }

    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Skicka" ]
          , className: "button-green"
          }

type DateInputField =
  { action   :: Maybe DateTime -> Action
  , value    :: Maybe DateTime
  , minDate  :: Maybe DateTime
  , maxDate  :: Maybe DateTime
  , disabled :: Boolean
  , label    :: String
  }

dateInput :: Self -> DateInputField -> JSX
dateInput self { action, value, minDate, maxDate, disabled, label } =
  Grid.row
    [ Grid.row_ [ DOM.label_ [ DOM.text label ] ]
    , Grid.row_
        [ DatePicker.datePicker
            { onChange: mkEffectFn1 \pickedDate -> do
                send self $ action $ toDateTime =<< toMaybe pickedDate
            , className: "pause-subscription--date-picker"
            , value: toNullable $ fromDateTime <$> value
            , format: "d.M.yyyy"
            , required: true
            , minDate: toNullable $ fromDateTime <$> minDate
            , maxDate: toNullable $ fromDateTime <$> maxDate
            , disabled
            , locale: "sv-SV"
            }
        ]
    ]
    $ Just { extraClasses: [ "mt2" ] }

submitForm :: State -> Props -> Effect Unit
submitForm { startDate: Just start, endDate: Just end } props@{ userUuid, subsno } = do
  loginResponse <- Login.loadToken
  case loginResponse of
    Just { token } -> do
      props.onLoading
      Aff.launchAff_ do
        pausedSub <- Persona.pauseSubscription userUuid subsno start end token `catchError` case _ of
          err | Just (errData :: Persona.InvalidPauseDates) <- Persona.errorData err -> do
                  liftEffect $ props.onError errData.invalid_pause_dates.message
                  throwError err
              | otherwise -> do
                  Console.error "Unexpected error when pausing subscription."
                  liftEffect $ props.onError Persona.PauseInvalidUnexpected
                  throwError err
        liftEffect $ props.onSuccess pausedSub
    Nothing -> Console.error "Did not find token in local storage."
submitForm _ _ = Console.error "Pause subscription dates were not defined."
