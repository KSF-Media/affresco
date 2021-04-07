module KSF.InputField.Checkbox where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetChecked)
import React.Basic.Events (handler)

type Self = React.Self Props State

type Props =
  { type_           :: InputType
  , name            :: String
  , value           :: Maybe String
  , checked         :: Boolean
  , onChange        :: Boolean -> Effect Unit
  , label           :: Maybe String
  , required        :: Boolean
  }

type State =
  { }

data InputType = Checkbox

derive instance genericInputType :: Generic InputType _
instance showInputType :: Show InputType where
  show = toLower <<< genericShow

component :: React.Component Props
component = React.createComponent "InputCheckbox"

inputCheckbox :: Props -> JSX
inputCheckbox = React.make component
  { render, initialState }
  where
    initialState :: State
    initialState = { }

render :: Self -> JSX
render self@{ props, state } =
  DOM.div
    { className: "input-chekcbox--checkbox-container" <>
        if isNothing props.label then " input-field--no-label" else ""
    , children:
        -- The final order of the children is defined in css!
        [ case props.label of
             Just label -> inputLabel { label, nameFor: props.name } props.required
             _          -> mempty
        , DOM.input
            { type: show props.type_
            , name: props.name
            , value: fromMaybe "" props.value
            , checked: props.checked
            , onChange: handler targetChecked \maybeNewVal -> do
                props.onChange $ fromMaybe false maybeNewVal
            }
        ]
    }

type InputLabel =
  { label   :: String -- ^ What to show in UI
  , nameFor :: String -- ^ Which input field this is for
  }

inputLabel :: InputLabel -> Boolean -> JSX
inputLabel { label, nameFor} required =
  DOM.label
    { className: "input-field--input-label"
    , children: if required
                   then [ DOM.text $ label <> " *" ]
                   else [ DOM.text label ]
    , htmlFor: nameFor
    }
