module KSF.FormInputField where

import Prelude

import Data.Array (catMaybes, head, null, snoc)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Effect (Effect)
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, inputFieldErrorMessage, isValidOrNotInitialized, validateEmptyField, validateField)
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

type Self = React.Self Props State

-- TODO: Add validations
type Props =
  { label          :: String
  , type_          :: String
  , name           :: String
  , placeholder    :: String
  , value          :: Maybe String
  , onChange       :: Maybe String -> Effect Unit
  , validationError :: Maybe String
  }
type State = {}

formInputField :: Props -> JSX
formInputField = make component { initialState, render }


component :: React.Component Props
component = React.createComponent "FormInputField"

initialState :: State
initialState = {}

render :: Self -> JSX
render = inputField <<< _.props

inputField :: Props -> JSX
inputField props =
  DOM.div
    { className: "form-input-field--container"
    , children:
        [ inputLabel props.label props.name
        , DOM.input
            { type: props.type_
            , placeholder: props.label
            , name: props.name
            , value: fromMaybe "" props.value
            , onChange: handler targetValue props.onChange
            , className:
                if isJust props.validationError
                then "form-input--field-invalid-field"
                else mempty
            }
        ] `snoc` foldMap errorMessage props.validationError
    }
  where
    errorMessage :: String -> JSX
    errorMessage e =
      DOM.span
        { className: "form-input-field--invalid-text"
        , children: [ DOM.text e ]
        }

inputLabel :: String -> String -> JSX
inputLabel labelText labelFor =
  DOM.label
    { className: "form-input-field--input-label"
    , children: [ DOM.text labelText ]
    , htmlFor: labelFor
    }
