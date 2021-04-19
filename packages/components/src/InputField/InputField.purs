module KSF.InputField where

import Prelude

import Data.Array (snoc)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Nub, class Union)
import React.Basic (JSX)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import Record as Record

foreign import generateIdNumber :: Effect Int

type Self = React.Self (Record Props) State

type Props =
  ( type_           :: InputType
  , placeholder     :: String
  , name            :: String
  , value           :: Maybe String
  , onChange        :: Maybe String -> Effect Unit
  , label           :: Maybe String
  , validationError :: Maybe String
  , disabled        :: Boolean
  )

type DefaultProps =
  ( disabled        :: Boolean
  )

type State =
  { inputValue :: String }

data InputType = Text | Password | Email | Radio

derive instance genericInputType :: Generic InputType _
instance showInputType :: Show InputType where
  show = toLower <<< genericShow

derive instance eqInputField :: Eq InputType

component :: React.Component (Record Props)
component = React.createComponent "InputField"

inputField :: forall attrs attrs_ . Union attrs DefaultProps attrs_ => Nub attrs_ Props => Record attrs -> JSX
inputField userProps = React.make component
  { render, didMount, initialState } $ Record.merge userProps defaultProps
  where
    defaultProps :: Record DefaultProps
    defaultProps =
      { disabled: false
      }

    didMount { props, setState } = when (isJust props.value) $
      setState \s -> s { inputValue = fromMaybe "" props.value }

    initialState :: State
    initialState = { inputValue: "" }

render :: Self -> JSX
render self@{ props, state } =
  DOM.div
    { className: classNameFromInputType props.type_ <>
        if isNothing props.label then " input-field--no-label" else ""
    , children:
        -- The final order of the children is defined in css!
        [ case props.label of
             Just label -> inputLabel { label, nameFor: id }
             _          -> mempty
        , DOM.input
            { type: show props.type_
            , id: id
            , placeholder: props.placeholder
            , name: props.name
            , value: fromMaybe state.inputValue props.value
            , onChange: handler targetValue \maybeNewVal -> do
                self.setState _ { inputValue = fromMaybe "" maybeNewVal }
                props.onChange maybeNewVal
            , className:
                if isJust props.validationError
                then "input-field--invalid-field"
                else mempty
            , disabled: props.disabled
            }
        ] `snoc` foldMap errorMessage props.validationError
    }
  where
    id = case props.label of
      Just _ -> props.name <> "-" <> show (unsafePerformEffect generateIdNumber)
      _ -> ""

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
