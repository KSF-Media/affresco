module KSF.InputField.Component where

import Prelude

import Data.Array (snoc)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.String (toLower)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

type Self = React.Self Props State

type Props =
  { type_           :: InputType
  , placeholder     :: String
  , name            :: String
  , value           :: Maybe String
  , onChange        :: Maybe String -> Effect Unit
  , label           :: String
  , validationError :: Maybe String
  }

type State =
  { inputValue :: String }

data InputType = Text | Password | Email | Radio

derive instance genericInputType :: Generic InputType _
instance showInputType :: Show InputType where
  show = toLower <<< genericShow

component :: React.Component Props
component = React.createComponent "InputField"

inputField :: Props -> JSX
inputField = React.make component
  { render, didMount, initialState }
  where
    didMount { props, setState } = when (isJust props.value) $
      setState \s -> s { inputValue = fromMaybe "" props.value }

    initialState :: State
    initialState = { inputValue: "" }

render :: Self -> JSX
render self@{ props, state } =
  DOM.div
    { className: classNameFromInputType props.type_
    , children:
        [ inputLabel props.label props.name
        , DOM.input
            { type: show props.type_
            , placeholder: props.label
            , name: props.name
            , value: state.inputValue
            , onChange: handler targetValue \maybeNewVal -> do
                self.setState _ { inputValue = fromMaybe "" maybeNewVal }
                props.onChange maybeNewVal
            , className:
                if isJust props.validationError
                then "input-field--invalid-field"
                else mempty
            }
        ] `snoc` foldMap errorMessage props.validationError
    }

classNameFromInputType :: InputType -> String
classNameFromInputType inputType = case inputType of
                                     Radio -> "input-field--radio-container"
                                     _     -> "input-field--container"
errorMessage :: String -> JSX
errorMessage e =
  DOM.span
    { className: "input-field--invalid-text"
    , children: [ DOM.text e ]
    }

inputLabel :: String -> String -> JSX
inputLabel labelText labelFor =
  DOM.label
    { className: "input-field--input-label"
    , children: [ DOM.text labelText ]
    , htmlFor: labelFor
    }
