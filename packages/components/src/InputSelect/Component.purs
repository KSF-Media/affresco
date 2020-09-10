module KSF.InputSelect.Component where

import Prelude

import Data.Array (snoc)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String (toLower)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetChecked)
import React.Basic.Events (handler)

type Self = React.Self Props State

type Props =
  { type_           :: InputType
  , name            :: String
  , value           :: Maybe String
  , checked         :: Maybe Boolean
  , onChange        :: Maybe Boolean -> Effect Unit
  , label           :: Maybe String
  , required        :: Maybe Boolean
  }

type State =
  { checked :: Boolean }

data InputType = Radio | Checkbox

derive instance genericInputType :: Generic InputType _
instance showInputType :: Show InputType where
  show = toLower <<< genericShow

component :: React.Component Props
component = React.createComponent "InputSelect"

inputSelect :: Props -> JSX
inputSelect = React.make component
  { render, didMount, initialState }
  where
    didMount { props, setState } = when (isJust props.checked) $
      setState \s -> s { checked = fromMaybe false props.checked }

    initialState :: State
    initialState = { checked: false }

render :: Self -> JSX
render self@{ props, state } =
  DOM.div
    { className: classNameFromInputType props.type_ <>
        if isNothing props.label then " input-field--no-label" else ""
    , children:
        -- The final order of the children is defined in css!
        [ case props.label of
             Just label -> inputLabel { label, nameFor: props.name }
             _          -> mempty
        , DOM.input
            { type: show props.type_
            , name: props.name
            , value: fromMaybe "" props.value
            , onChange: handler targetChecked \maybeNewVal -> do
                self.setState _ { checked = fromMaybe false maybeNewVal }
                props.onChange maybeNewVal
            , className:
                if isJust props.required
                then "input-field--invalid-field"
                else mempty
            }
        ]
    }

classNameFromInputType :: InputType -> String
classNameFromInputType inputType = case inputType of
                                     Checkbox -> "input-select--checkbox-container"
                                     _        -> "input-field--container"
errorMessage :: String -> JSX
errorMessage e =
  DOM.span
    { className: "input-field--invalid-text"
    , children: [ DOM.text e ]
    }

type InputLabel =
  { label   :: String -- ^ What to show in UI
  , nameFor :: String -- ^ Which input field this is for
  }

inputLabel :: InputLabel -> JSX
inputLabel { label, nameFor } =
  DOM.label
    { className: "input-field--input-label"
    , children: [ DOM.text label ]
    , htmlFor: nameFor
    }
