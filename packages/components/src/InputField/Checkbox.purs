module KSF.InputField.Checkbox where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Monoid (guard)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import KSF.InputField (generateIdNumber)
import Prim.Row (class Nub, class Union)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetChecked)
import React.Basic.Events (handler)
import Record as Record

type PropsRow =
  ( type_           :: InputType
  , name            :: String
  , value           :: String
  , checked         :: Boolean
  , onChange        :: Boolean -> Effect Unit
  , label           :: Maybe String
  , labelJSX        :: Maybe JSX
  , required        :: Boolean
  , id              :: String
  , checkboxFirst   :: Boolean
  )

type OptionalPropsRow =
  ( value           :: String
  , label           :: Maybe String
  , labelJSX        :: Maybe JSX
  , required        :: Boolean
  , id              :: String
  , checkboxFirst   :: Boolean
  )

type Props = Record PropsRow

data InputType = Checkbox

derive instance genericInputType :: Generic InputType _
instance showInputType :: Show InputType where
  show = toLower <<< genericShow

inputCheckbox
  :: forall attrs attrs_. Union attrs OptionalPropsRow attrs_
  => Nub attrs_ PropsRow
  => Record attrs
  -> JSX
inputCheckbox props = render $ Record.merge props defaultProps
  where
    defaultProps :: Record OptionalPropsRow
    defaultProps =
      { value: ""
      , label: Nothing
      , labelJSX: Nothing
      , required: false
      , id: ""
      , checkboxFirst: false
      }

render :: Props -> JSX
render props =
  DOM.div
    { className: "input-checkbox--checkbox-container" <>
        if isNothing props.label && isNothing props.labelJSX then " input-field--no-label" else ""
    , children:
        -- The final order of the children is defined in css!
        [ guard (not props.checkboxFirst) inputLabel
        , DOM.input
            { type: show props.type_
            , name: props.name
            , value: props.value
            , checked: props.checked
            , onChange: handler targetChecked \maybeNewVal -> do
                props.onChange $ fromMaybe false maybeNewVal
            , id
            }
        , guard props.checkboxFirst inputLabel
        ]
    }
  where
    id = case props.id of
      "" -> case inputLabelContent of
        Just _ -> props.name <> "-" <> show (unsafePerformEffect generateIdNumber)
        _ -> ""
      _ -> props.id
    inputLabelContent = props.labelJSX <|>
                        (map (\label -> DOM.text $ label <> if props.required then " *" else "") props.label)
    inputLabel =
      foldMap (\content ->
                  DOM.label
                    { className: "input-field--input-label"
                    , children: [ content ]
                    , htmlFor: id
                    }) inputLabelContent
