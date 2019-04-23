module KSF.InputField.Component where

import Prelude

import Data.Maybe (Maybe, fromMaybe, isJust)
import Effect (Effect)
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events as Events
import React.Basic (JSX)
import React.Basic as React
import Data.Array ((:))
import React.Basic.DOM as DOM

type Props =
  { type_ :: String
  , placeholder :: String
  , name :: String
  , required :: Boolean
  , children :: Array JSX
  , defaultValue :: Maybe String
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

inputField :: Props -> JSX
inputField = React.make component
  { render, didMount, initialState }
  where
    didMount { props, setState } = when (isJust props.defaultValue) do
      setState \s -> s { inputValue = fromMaybe "" props.defaultValue }

    initialState :: State
    initialState = { inputValue: "" }

component :: React.Component Props
component = React.createComponent "InputField"

render :: forall r. { props :: Props, state :: State, setState :: SetState | r } -> JSX
render { state, setState, props } =
  DOM.div
    { className: "input-field"
    , children: input : props.children
    }
  where
    input =
      DOM.input
        { type: props.type_
        , placeholder: props.placeholder
        , name: props.name
        , value: state.inputValue
        , required: props.required
        , onChange
        }

    onChange =
      Events.handler
        (preventDefault >>> Events.merge { targetValue })
        \{ targetValue } -> do
          let newValue = fromMaybe "" targetValue
          setState _ { inputValue = newValue }
          props.onChange newValue
