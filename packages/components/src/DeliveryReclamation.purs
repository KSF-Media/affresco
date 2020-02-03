module KSF.DeliveryReclamation where

import Prelude

import Control.Alt ((<|>))
import Data.DateTime (DateTime, adjust)
import Data.Either (Either(..))
import Data.JSDate (fromDateTime)
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (toNullable)
import Data.Time.Duration as Time.Duration
import Data.Validation.Semigroup (unV)
import Data.String.Read
import DatePicker.Component as DatePicker
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import KSF.Grid as Grid
import KSF.InputField.Component as InputField
import KSF.User as User
import KSF.ValidatableForm as VF
import KSF.CountryDropDown (countryDropDown)
import Persona as Persona
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)

type State =
  { publicationDate    :: Maybe DateTime
  , claim              :: Maybe User.DeliveryReclamationClaim
  , maxPublicationDate :: Maybe DateTime
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

deliveryReclamation :: Props -> JSX
deliveryReclamation = make component { initialState, render, didMount }

initialState :: State
initialState =
  { publicationDate: Nothing
  , claim: Nothing
  , maxPublicationDate: Nothing
  }

component :: React.Component Props
component = React.createComponent "DeliveryReclamation"

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  self.setState _ { maxPublicationDate = Just now }

render :: Self -> JSX
render self@{ state: { publicationDate, claim, maxPublicationDate }} =
  DOM.div
    { className: "clearfix delivery-reclamation--container"
    , children:
        [ Grid.row_
           [ DOM.div
               { className: "col col-11"
               , children: [ DOM.h3_ [ DOM.text "Text here" ] ]
               }
           , DOM.div
               { className: "col-1 flex delivery-reclamation--close-icon"
               , children: [ DOM.div { className: "close-icon" } ]
               , onClick: handler_ self.props.onCancel
               }
           , deliveryReclamationForm
           ]
        ]
    }
  where
    deliveryReclamationForm =
      DOM.form
          { onSubmit: handler preventDefault (\_ -> pure unit)
          , children:
              [ publicationDayInput
              , claimExtensionInput
              , DOM.div
                  { children: [ ]
                  , className: "mt2 clearfix"
                  }
              ]
          }

    publicationDayInput = dateInput self

    claimExtensionInput =
      InputField.inputField
        { type_: "radio"
        , placeholder: "Placeholder"
        , name: "claim"
        , onChange: \newClaim ->
                      let
                        updateClaim x = self.setState _ { claim = x }
                        parsed = read <$> newClaim
                      in
                        case parsed of
                          Just value -> updateClaim value
                          Nothing    -> updateClaim $ Nothing
        , value: Nothing
        , label: "Text here"
        , validationError: Nothing
        }

dateInput :: Self -> JSX
dateInput self =
  DatePicker.datePicker
      { onChange: (_ >>= \publicationDate -> self.setState (\currentState -> currentState{publicationDate = publicationDate}))
      , className: "delivery-reclamation--date-picker"
      , value: toNullable $ fromDateTime <$> self.state.publicationDate
      , format: "d.M.yyyy"
      , required: true
      , minDate: toNullable $ Nothing
      , maxDate: toNullable $ fromDateTime <$> self.state.maxPublicationDate
      , disabled : false
      , locale: "sv-FI"
      }
