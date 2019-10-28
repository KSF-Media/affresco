module KSF.TemporaryAddressChange.Component where

import Prelude

import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..))
import Data.JSDate (fromDateTime)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Nullable (toNullable)
import Data.Time.Duration as Time.Duration
import DatePicker.Component as DatePicker
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import KSF.Grid as Grid
import KSF.InputField.Component as InputField
import KSF.User as User
import KSF.ValidatableForm (class ValidatableField, inputFieldErrorMessage, validateEmptyField, validateField, validateZipCode)
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)

type State =
  { startDate     :: Maybe DateTime
  , minStartDate  :: Maybe DateTime
  , endDate       :: Maybe DateTime
  , minEndDate    :: Maybe DateTime
  , streetAddress :: Maybe String
  , zipCode       :: Maybe String
  }

type Self = React.Self Props State

type Props =
  { subsno    :: Int
  , userUuid  :: Persona.UUID
  , onCancel  :: Effect Unit
  , onLoading :: Effect Unit
  , onSuccess :: Persona.Subscription -> Effect Unit
  , onError   :: Persona.InvalidDateInput -> Effect Unit
  }

data Action
  = SetStartDate (Maybe DateTime)
  | SetMinStartDate (Maybe DateTime)
  | SetEndDate (Maybe DateTime)

data AddressChangeFields
  = StreetAddress
  | Zip
  | City
instance validatableFieldAddressChangeFields :: ValidatableField AddressChangeFields where
  validateField field value _serverErrors = case field of
    StreetAddress -> validateEmptyField field "Adress krävs." value
    City          -> validateEmptyField field "Stad krävs." value
    Zip           -> validateZipCode field value

temporaryAddressChange :: Props -> JSX
temporaryAddressChange = make component { initialState, render, didMount }

initialState :: State
initialState =
  { startDate: Nothing
  , minStartDate: Nothing
  , endDate: Nothing
  , minEndDate: Nothing
  , streetAddress: Nothing
  , zipCode: Nothing
  }

component :: React.Component Props
component = React.createComponent "TemporaryAddressChange"

-- | Minimum temporary address change period is one week
calcMinEndDate :: Maybe DateTime -> Maybe DateTime
calcMinEndDate Nothing = Nothing
calcMinEndDate (Just startDate) = do
  let week = Time.Duration.Days 7.0
  adjust week startDate

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  -- We set the minimum start date two days ahead because of system issues.
  -- TODO: This could be set depending on the time of day
  let dayAfterTomorrow = adjust (Time.Duration.Days 2.0) now
  self.setState _ { minStartDate = dayAfterTomorrow }

render :: Self -> JSX
render self =
  DOM.div
    { className: "clearfix temporary-address-change--container"
    , children:
        [ Grid.row_
           [ DOM.div
               { className: "col col-11"
               , children: [ DOM.h3_ [ DOM.text "Gör tillfällig adressändring" ] ]
               }
           , DOM.div
               { className: "col-1 flex temporary-address-change--close-icon"
               , children: [ DOM.div { className: "close-icon" } ]
               , onClick: handler_ self.props.onCancel
               }
           , addressChangeForm
           ]
        ]
    }
  where
    addressChangeForm =
      DOM.form
          { onSubmit: handler preventDefault (\_ -> submitForm self.state self.props)
          , children:
              [ startDayInput
              , endDayInput
              , addressInput
              , zipInput
              , cityInput
              , DOM.div
                  { children: [ submitFormButton ]
                  , className: "mt2 clearfix"
                  }
              ]
          }

    startDayInput =
      dateInput
        self
        { action: \newStartDate ->
                    self.setState _
                      { startDate = newStartDate
                      , minEndDate = calcMinEndDate newStartDate
                      }
        , value: self.state.startDate
        , minDate: self.state.minStartDate
        , maxDate: Nothing
        , disabled: false
        , label: "Börjar från"
        }

    endDayInput =
      dateInput
        self
        { action: \newEndDate -> self.setState _ { endDate = newEndDate }
        , value: self.state.endDate
        , minDate: self.state.minEndDate
        , maxDate: Nothing
        , disabled: isNothing self.state.startDate
        , label: "Avslutas"
        }

    addressInput =
      InputField.inputField
        { type_: "text"
        , placeholder: "Gatuadress"
        , name: "address"
        , onChange: \newAddress -> self.setState _ { streetAddress = newAddress }
        , value: Nothing
        , label: "Gatuadress"
        , validationError: inputFieldErrorMessage $ validateField StreetAddress self.state.streetAddress []
        }

    zipInput =
      InputField.inputField
        { type_: "text"
        , placeholder: "Postnummer"
        , name: "zipCode"
        , onChange: \newZip -> self.setState _ { zipCode = newZip }
        , value: Nothing
        , label: "Postnummer"
        , validationError: inputFieldErrorMessage $ validateField Zip self.state.zipCode []
        }

    -- A dummy input field for UX pleasure
    -- NOTE: The correct city will be eventually inferred by the zip code
    cityInput =
      InputField.inputField
        { type_: "text"
        , placeholder: "Stad"
        , name: "city"
        , onChange: \_ -> pure unit
        , value: Nothing
        , validationError: Nothing
        , label: "Stad"
        }

    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Skicka" ]
          , className: "button-green"
          }

type DateInputField =
  { action   :: Maybe DateTime -> Effect Unit
  , value    :: Maybe DateTime
  , minDate  :: Maybe DateTime
  , maxDate  :: Maybe DateTime
  , disabled :: Boolean
  , label    :: String
  }

dateInput :: Self -> DateInputField -> JSX
dateInput self { action, value, minDate, maxDate, disabled, label } =
  Grid.row
    [ Grid.row_ [ DOM.label_ [ DOM.text label ] ]
    , Grid.row_
        [ DatePicker.datePicker
            { onChange: (action =<< _)
            , className: "temporary-address-change--date-picker"
            , value: toNullable $ fromDateTime <$> value
            , format: "d.M.yyyy"
            , required: true
            , minDate: toNullable $ fromDateTime <$> minDate
            , maxDate: toNullable $ fromDateTime <$> maxDate
            , disabled
            , locale: "sv-FI"
            }
        ]
    ]
    $ Just { extraClasses: [ "mb2" ] }

submitForm :: State -> Props -> Effect Unit
submitForm { startDate: Just start, endDate: Just end, streetAddress, zipCode } props@{ userUuid, subsno } = do
  props.onLoading
  Aff.launchAff_ do
    -- TODO: FIX FROMMAYBES!
    User.temporaryAddressChange userUuid subsno start end (fromMaybe "" streetAddress) (fromMaybe "" zipCode) >>=
      case _ of
        Right sub -> liftEffect $ props.onSuccess sub
        Left invalidDateInput -> liftEffect $ props.onError invalidDateInput
submitForm _ _ = Console.error "Temporary address change dates were not defined."
