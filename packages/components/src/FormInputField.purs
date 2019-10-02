module KSF.FormInputField where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
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
--  , validatedInput :: ValidatedForm (Maybe String)
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
            -- , className: if isValidOrNotInitialized a.validatedInput
            --                then ""
            --                else "form-input-field--invalid-field"
            }
        ] -- `snoc` foldMap errorMessage (inputFieldErrorMessage a.validatedInput)
    }
  where
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
