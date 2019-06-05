module KSF.TemporaryAddressChange.Component where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.DateTime (DateTime, adjust)
import Data.JSDate (fromDateTime, toDateTime)
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (toMaybe, toNullable)
import Data.Time.Duration as Time.Duration
import DatePicker.Component as DatePicker
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Effect.Uncurried (mkEffectFn1)
import KSF.Grid as Grid
import KSF.InputField.Component as InputField
import KSF.Login.Component as Login
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import React.Basic.Extended (Style)
import React.Basic.Extended as React.Extended

foreign import temporaryAddressChangeStyles :: Style

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
  let tomorrow = adjust (Time.Duration.Days 1.0) now
  self.setState _ { minStartDate = tomorrow }

render :: Self -> JSX
render self = withStyles $
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
        , label: "Skall starta igen"
        }

    addressInput =
      Grid.row
        [ Grid.row_ [ DOM.label_ [ DOM.text "Adress*" ] ]
        , Grid.row_
            [ React.element
              InputField.component
              { type_: "text"
              , placeholder: "Adress"
              , name: "address"
              , required: true
              , children: []
              , onChange: \newAddress -> self.setState _ { streetAddress = newAddress }
              , defaultValue: Nothing
              }
            ]
        ]
        $ Just { extraClasses: [ "mt2" ] }

    zipInput =
      Grid.row
        [ Grid.row_ [ DOM.label_ [ DOM.text "Postnummer*" ] ]
        , Grid.row_
            [ React.element
              InputField.component
              { type_: "text"
              , placeholder: "Postnummer"
              , name: "zipCode"
              , required: true
              , children: []
              , onChange: \newZip -> self.setState _ { zipCode = newZip }
              , defaultValue: Nothing
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
            , locale: "sv-SV"
            }
        ]
    ]
    $ Just { extraClasses: [ "mt2" ] }

withStyles :: JSX -> JSX
withStyles = React.Extended.requireStyle temporaryAddressChangeStyles

submitForm :: State -> Props -> Effect Unit
submitForm { startDate: Just start, endDate: Just end, streetAddress, zipCode } props@{ userUuid, subsno } = do
  loginResponse <- Login.loadToken
  case loginResponse of
    Just { token } -> do
      props.onLoading
      Aff.launchAff_ do
        pausedSub <- Persona.temporaryAddressChange userUuid subsno start end streetAddress zipCode token `catchError` case _ of
          err | Just (errData :: Persona.InvalidDates) <- Persona.errorData err -> do
                  liftEffect $ props.onError errData.invalid_param.message
                  throwError err
              | otherwise -> do
                  Console.error "Unexpected error when making temporary address change."
                  liftEffect $ props.onError Persona.InvalidUnexpected
                  throwError err
        liftEffect $ props.onSuccess pausedSub
    Nothing -> Console.error "Did not find token in local storage."
submitForm _ _ = Console.error "Temporary address change dates were not defined."
