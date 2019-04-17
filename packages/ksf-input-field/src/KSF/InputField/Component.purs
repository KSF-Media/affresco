module KSF.InputField.Component where

import Prelude

import Data.Maybe (Maybe, fromMaybe, isJust)
import Effect (Effect)
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events as Events
import React.Basic.Extended (JSX)
import React.Basic.Extended as React
import Data.Array ((:))
import Effect.Uncurried (EffectFn1)
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

component :: React.Component Props
component = React.component { displayName: "InputField", render, receiveProps, initialState }
  where
    receiveProps { props, setState } = when (isJust props.defaultValue) do
      setState \s -> s { inputValue = fromMaybe "" props.defaultValue }

    initialState :: State
    initialState = { inputValue: "" }

render :: forall r. { props :: Props, state :: State, setState :: SetState | r } -> JSX
render { state, setState, props } =
  inputField
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


type Attributes =
  { type_ :: String
  , placeholder :: String
  , name :: String
  , value :: String
  , required :: Boolean
  , onChange :: EffectFn1 Events.SyntheticEvent Unit
  , children :: Array JSX
  }

inputField :: Attributes -> JSX
inputField attrs =
  DOM.div
    { className: "input-field"
    , children: input : attrs.children
    }
  where
    input =
      DOM.input
        { type: attrs.type_
        , placeholder: attrs.placeholder
        , name: attrs.name
        , value: attrs.value
        , required: attrs.required
        , onChange: attrs.onChange
        }
