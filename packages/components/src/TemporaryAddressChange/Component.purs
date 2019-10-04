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
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Events as Events

type State =
  { startDate     :: Maybe DateTime
  , minStartDate  :: Maybe DateTime
  , endDate       :: Maybe DateTime
  , minEndDate    :: Maybe DateTime
  , streetAddress :: String
  , zipCode       :: String
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

temporaryAddressChange :: Props -> JSX
temporaryAddressChange = make component { initialState, render, didMount }

initialState :: State
initialState =
  { startDate: Nothing
  , minStartDate: Nothing
  , endDate: Nothing
  , minEndDate: Nothing
  , streetAddress: ""
  , zipCode: ""
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
      Grid.row
        [ Grid.row_ [ DOM.label_ [ DOM.text "Gatuadress*" ] ]
        , Grid.row_
            [ InputField.inputField
              { type_: "text"
              , placeholder: "Gatuadress"
              , name: "address"
              , children: []
              , onChange: \newAddress -> self.setState _ { streetAddress = fromMaybe "" newAddress }
              , value: Nothing
              , label: "Gatuadress"
              , validationError: Nothing
              }
            ]
        ]
        $ Just { extraClasses: [ "mt2" ] }

    zipInput =
      Grid.row
        [ Grid.row_ [ DOM.label_ [ DOM.text "Postnummer*" ] ]
        , Grid.row_
            [ DOM.input
              { type: "text"
              , placeholder: "Postnummer"
              , name: "zipCode"
              , required: true
              , value: self.state.zipCode
              , onChange:
                  Events.handler
                    (preventDefault >>> Events.merge { targetValue })
                    \{ targetValue: newZip } -> self.setState _ { zipCode = fromMaybe "" newZip }
              , pattern: "\\d+"
              , title: "Endast siffror"
              }
            ]
        ]
        $ Just { extraClasses: [ "mt2" ] }

    -- A dummy input field for UX pleasure
    -- NOTE: The correct city will be eventually inferred by the zip code
    cityInput =
      Grid.row
        [ Grid.row_ [ DOM.label_ [ DOM.text "Stad" ] ]
        , Grid.row_
            [ InputField.inputField
              { type_: "text"
              , placeholder: "Stad"
              , name: "city"
              , children: []
              , onChange: \_ -> pure unit
              , value: Nothing
              , validationError: Nothing
              , label: "Stad"
              }
            ]
        ]
        $ Just { extraClasses: [ "mt2" ] }

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
    $ Just { extraClasses: [ "mt2" ] }

submitForm :: State -> Props -> Effect Unit
submitForm { startDate: Just start, endDate: Just end, streetAddress, zipCode } props@{ userUuid, subsno } = do
  props.onLoading
  Aff.launchAff_ do
    User.temporaryAddressChange userUuid subsno start end streetAddress zipCode >>=
      case _ of
        Right sub -> liftEffect $ props.onSuccess sub
        Left invalidDateInput -> liftEffect $ props.onError invalidDateInput
submitForm _ _ = Console.error "Temporary address change dates were not defined."
