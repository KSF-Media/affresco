module KSF.InputField.Component where

import Prelude

import Data.Maybe (fromMaybe)
import Effect (Effect)
import KSF.InputField.View as View
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events as Events

type Props =
  { type_ :: String
  , placeholder :: String
  , name :: String
  , required :: Boolean
  , children :: Array JSX
  , onChange :: String -> Effect Unit
  }

type State =
  { inputValue :: String }


type InputFieldAttributes =
  { type_ :: String
  , name :: String
  , placeholder :: String
  , required :: Boolean
  }

type SetState = (State -> State) -> Effect Unit

component :: React.Component Props
component = React.component { displayName: "InputField", render, receiveProps, initialState }
  where
    receiveProps _ = pure unit

    initialState :: State
    initialState = { inputValue: "" }

render :: forall r. { props :: Props, state :: State, setState :: SetState | r } -> JSX
render { state, setState, props } =
  View.inputField
    { type_: props.type_
    , placeholder: props.placeholder
    , name: props.name
    , value: state.inputValue
    , required: props.required
    , children: props.children
    , onChange
    }
  where
    onChange =
      Events.handler
        (preventDefault >>> Events.merge { targetValue })
        \{ targetValue } -> do
          let newValue = fromMaybe "" targetValue
          setState _ { inputValue = newValue }
          props.onChange newValue
