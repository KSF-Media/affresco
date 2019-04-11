module KSF.PauseSubscription.Component where

import Prelude

import Data.DateTime (DateTime(..))
import Data.Function.Uncurried (Fn0, runFn0)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
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
  { onChange :: Effect Unit
  , className :: String
  , value :: Maybe DateTime
  }

type Self = React.Self Props State

type Props =
  { subsno :: Int
  , onCancel :: Effect Unit
  }

type State =
  { startDay :: Maybe String
  , endDay   :: Maybe String
  , now :: DateTime
  }

data Action
  = SetStartDay (Maybe String)
  | SetEndDay (Maybe String)
  | SetNow DateTime

pauseSubscription :: Props -> JSX
pauseSubscription = make component { initialState, render }

component :: React.Component Props
component = React.createComponent "PauseSubscription"

initialState :: State
initialState =
  { startDay: Nothing
  , endDay: Nothing
  , now: Nothing
  }

update :: Self -> Action -> StateUpdate Props State
update self action = Update $ case action of
  SetStartDay newStartDay -> self.state { startDay = newStartDay }
  SetEndDay newEndDay -> self.state { endDay = newEndDay }
  SetNow now -> self.state { now = now }

send :: Self -> Action -> Effect Unit
send = runUpdate update

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  send self $ SetNow $ Just now

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
            , element datePicker
                        { onChange: Console.log "waaat"
                        , className: "yolo"
                        , value: self.state.now -- Nullable?
                        }
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

    startDayInput = dateInput "startDate" "Börjar från" SetStartDay
    endDayInput   = dateInput "endDate" "Skall starta igen" SetEndDay

    dateInput :: String -> String -> (Maybe String -> Action) -> JSX
    dateInput name labelText action =
      Grid.row
        [ Grid.row [ DOM.label_ [ DOM.text labelText ] ] Nothing
        , Grid.columnThird $
            DOM.input
              { type: "text"
              , required: true
              , onChange: handler targetValue $ action >>> send self
              , name
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
