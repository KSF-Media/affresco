module KSF.PauseSubscription.Component where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console as Console
import React.Basic (JSX, StateUpdate(..), make, send)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended

foreign import pauseSubscriptionStyles :: Style

type Self = React.Self Props State Void

type Props =
  { subsno :: Int
  , onCancel :: Effect Unit
  }

type State =
  { startDay :: Maybe String
  , endDay   :: Maybe String
  }

data Action
  = SetStartDay (Maybe String)
  | SetEndDay (Maybe String)

pauseSubscription :: Props -> JSX
pauseSubscription = make component { initialState, render, update }

component :: React.Component Props
component = React.createComponent "PauseSubscription"

initialState :: State
initialState =
  { startDay: Nothing
  , endDay: Nothing
  }

update :: Self -> Action -> StateUpdate Props State Action
update self action = Update $ case action of
  SetStartDay newStartDay -> self.state { startDay = newStartDay }
  SetEndDay newEndDay -> self.state { endDay = newEndDay }

render :: Self -> JSX
render self =
  React.Extended.requireStyle
    pauseSubscriptionStyles
    $ DOM.div
        { className: "clearfix pause-subscription--container"
        , children:
            [ DOM.h3_ [ DOM.text "Uppehåll på prenumarationen" ]
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
              , submitFormButton
              , cancelPausing
              ]
          }

    startDayInput =
        DOM.div
          { className: "clearfix"
          , children:
              [ DOM.label
                  { className: "col col-12"
                  , children: [ DOM.text "Börjar från" ]
                  }
              , DOM.input
                  { className: "col col-3" -- TODO: narrower views
                  , required: true
                  , onChange: handler targetValue $ SetStartDay >>> send self
                  , name: "startDate"
                  }
              ]
            }
    endDayInput =
        DOM.div
          { className: "clearfix"
          , children:
              [ DOM.label
                  { className: "col col-12"
                  , children: [ DOM.text "Skall starta igen" ]
                  }
              , DOM.input
                  { className: "col col-3" -- TODO: narrower views
                  , required: true
                  , onChange: handler targetValue $ SetEndDay >>> send self
                  , name: "endDate"
                  }
              ]
            }

    submitFormButton =
        DOM.input
          { type: "submit"
          , value: "Skicka"
          }

    cancelPausing =
      DOM.div
        { className: ""
        , children: [ DOM.text "Avbryt" ]
        , onClick: handler_ self.props.onCancel
        }

submitForm :: Maybe String -> Maybe String -> Int -> Effect Unit
submitForm (Just startDay) (Just endDay) subsno =
  Console.log $ startDay
submitForm _ _ _ = pure unit
