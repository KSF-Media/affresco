module KSF.PauseSubscription.Component where

import Prelude

import Data.Date (Date)
import Data.Function.Uncurried (Fn0, runFn0)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import KSF.Grid as Grid
import React.Basic (JSX, ReactComponent, StateUpdate(..), element, make, runUpdate)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended

foreign import pauseSubscriptionStyles :: Style
foreign import datePicker_ :: Fn0 (ReactComponent DatePickerProps)

type DatePickerProps =
  { onChange  :: EffectFn1 Date Unit
  , className :: String
  , value     :: Nullable Date
  , format :: String
  , required :: Boolean
  }

type Self = React.Self Props State

type Props =
  { subsno   :: Int
  , onCancel :: Effect Unit
  }

type State =
  { startDay :: Maybe String
  , endDay   :: Maybe String
  , startDate :: Maybe Date
  , endDate :: Maybe Date
  , now      :: Maybe Date
  }

data Action
  = SetStartDate Date
  | SetEndDate Date

pauseSubscription :: Props -> JSX
pauseSubscription = make component { initialState, render }

component :: React.Component Props
component = React.createComponent "PauseSubscription"

initialState :: State
initialState =
  { startDay: Nothing
  , endDay: Nothing
  , startDate: Nothing
  , endDate: Nothing
  , now: Nothing
  }

update :: Self -> Action -> StateUpdate Props State
update self action = Update $ case action of
  SetStartDate newStartDate -> self.state { startDate = Just newStartDate }
  SetEndDate newEndDate -> self.state { endDate = Just newEndDate }

send :: Self -> Action -> Effect Unit
send = runUpdate update

render :: Self -> JSX
render self =
  React.Extended.requireStyle
    pauseSubscriptionStyles
    $ DOM.div
        { className: "clearfix pause-subscription--container"
        , children:
            [ Grid.row2
                (DOM.h3_ [ DOM.text "Uppehåll på prenumarationen" ])
                (DOM.div
                   { className: "flex pause-subscription--close-icon"
                   , children: [ DOM.div { className: "close-icon" } ]
                   , onClick: handler_ self.props.onCancel
                   })
                Nothing
            , pauseForm
            ]
        }
  where
    pauseForm =
      DOM.form
          { onSubmit: handler preventDefault (\_ -> submitForm self.state.startDay self.state.endDay self.props.subsno)
          , children:
              [ startDayInput
              , endDayInput
              , DOM.div
                  { children: [ submitFormButton ]
                  , className: "mt2 clearfix"
                  }
              ]
          }

    startDayInput = dateInput self.state.startDate "Börjar från" SetStartDate
    endDayInput   = dateInput self.state.endDate "Skall starta igen" SetEndDate

    dateInput :: Maybe Date -> String -> (Date -> Action) -> JSX
    dateInput date labelText setDate =
      Grid.row
        [ Grid.row [ DOM.label_ [ DOM.text labelText ] ] Nothing
        , Grid.columnThird
          $ element
              datePicker
                { onChange: mkEffectFn1 \pickedDate -> send self $ setDate pickedDate
                , className: "pause-subscription--date-picker"
                , value: toNullable date
                , format: "d.M.yyyy"
                , required: true
                }

        ]
        $ Just { extraClasses: [ "mt2" ] }

    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Skicka" ]
          , className: "button-green"
          }

submitForm :: Maybe String -> Maybe String -> Int -> Effect Unit
submitForm (Just startDay) (Just endDay) subsno =
  Console.log $ startDay
submitForm _ _ _ = pure unit

datePicker :: ReactComponent DatePickerProps
datePicker = runFn0 datePicker_
