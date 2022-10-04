module KSF.DeliveryReclamation where

import Prelude

import Data.Array (snoc)
import Data.Date (Date)
import Data.DateTime (DateTime, date, time)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.String.Read (read)
import Data.Time (Time, hour, minute)
import Data.UUID (UUID)
import DatePicker.Component as DatePicker
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import KSF.Api.Subscription (Subsno)
import KSF.Grid as Grid
import KSF.InputField as InputField
import KSF.User as User
import KSF.User.Cusno (Cusno)
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import KSF.Tracking as Tracking

type State =
  { publicationDate    :: Maybe Date
  , doorCode           :: Maybe String
  , claim              :: Maybe User.DeliveryReclamationClaim
  , maxPublicationDate :: Maybe Date
  , deliveryTroubleEnd :: Maybe Time
  , validationError    :: Maybe String
  }

type Self = React.Self Props State

type Props =
  { subsno    :: Subsno
  , end       :: Maybe DateTime
  , cusno     :: Cusno
  , userUuid  :: UUID
  , onCancel  :: Effect Unit
  , onLoading :: Effect Unit
  , onSuccess :: User.DeliveryReclamation -> Effect Unit
  , onError   :: User.InvalidDateInput -> Effect Unit
  }

deliveryReclamation :: Props -> JSX
deliveryReclamation = make component { initialState, render, didMount }

initialState :: State
initialState =
  { publicationDate: Nothing
  , doorCode: Just ""
  , claim: Nothing
  , maxPublicationDate: Nothing
  , validationError: Nothing
  , deliveryTroubleEnd: Nothing
  }

component :: React.Component Props
component = React.createComponent "DeliveryReclamation"

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  when (maybe false (\end -> now < end && date now == date end) self.props.end) $
    self.setState _ { deliveryTroubleEnd = time <$> self.props.end}
  self.setState _ { maxPublicationDate = Just $ date now }

render :: Self -> JSX
render self@{ state: { publicationDate, doorCode, claim }} =
  DOM.div
    { className: "delivery-reclamation--container"
    , children:
        [ Grid.row
           [ DOM.div
               { children: [ DOM.h3_ [ DOM.text "Reklamation" ] ]
               }
           , DOM.div
               { className: "delivery-reclamation--close-icon"
               , children: [ DOM.div { className: "close-icon" } ]
               , onClick: handler_ self.props.onCancel
               }
           ]  { extraClasses: [ "mitt-konto--closable-row" ] }
        , deliveryReclamationForm
        ]
    }
  where
    deliveryReclamationForm =
      DOM.form
          { onSubmit: handler preventDefault (\_ -> submitForm publicationDate doorCode claim)
          , children:
              [ foldMap delayMessage self.state.deliveryTroubleEnd
              , publicationDayInput
              , claimExtensionInput
              , claimNewDeliveryInput
              , guard (claim == Just User.NewDelivery) doorCodeInput
              , eveningMessage
              ] `snoc`foldMap errorMessage self.state.validationError
                `snoc` DOM.div
                        { children: [ submitFormButton ]
                        , className: "delivery-reclamation--submit-container"
                        }
          }

    publicationDayInput = dateInput self self.state.publicationDate "Utgivningsdatum"

    eveningMessage =
      DOM.div
        { children: [ DOM.text "Obs! Reklamationer som är gjorda innan 16.30 delas i regel ut med följande möjliga tidningsutdelning." ] }

    claimExtensionInput =
      InputField.inputField
        { type_: InputField.Radio
        , placeholder: "Extension"
        , name: "claim"
        , onChange: radioButtonOnChange self
        , value: Just "Extension"
        , label: Just "Jag klarar mig utan den uteblivna tidningen, förläng i stället min prenumeration med en dag"
        , validationError: Nothing
        }

    delayMessage time =
      let msg = "Utdelningen är försenad på ert område och beräknas vara klar till klockan " <>
                (show $ fromEnum $ hour time) <> ":" <>
                (let m = fromEnum $ minute time
                 in if m < 10 then "0" <> show m else show m) <> "."
      in DOM.div { className: "delivery-reclamation--estimated-time"
                 , children: [ DOM.text msg ]
                 }

    claimNewDeliveryInput =
      InputField.inputField
        { type_: InputField.Radio
        , placeholder: "New delivery"
        , name: "claim"
        , onChange: radioButtonOnChange self
        , value: Just "NewDelivery"
        , label: Just "Jag vill att den uteblivna tidningen levereras till mig"
        , validationError: Nothing
        }

    doorCodeInput =
      InputField.inputField
        { type_: InputField.Text
        , placeholder: "Dörrkod"
        , name: "doorCode"
        , onChange: \doorCode' -> self.setState _ { doorCode = doorCode' }
        , value: self.state.doorCode
        , label: Just "Dörrkod"
        , validationError: Nothing
        }

    errorMessage =
      InputField.errorMessage

    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Skicka" ]
          , className: "button-green"
          }

    submitForm :: Maybe Date -> Maybe String -> Maybe User.DeliveryReclamationClaim -> Effect Unit
    submitForm (Just date') (Just doorCode') (Just claim') = do
      Aff.launchAff_ do
        createDeliveryReclamation date' doorCode' claim'
      where
        createDeliveryReclamation :: Date -> String -> User.DeliveryReclamationClaim -> Aff Unit
        createDeliveryReclamation date'' doorCode'' claim'' = do
          liftEffect $ self.props.onLoading
          User.createDeliveryReclamation self.props.userUuid self.props.subsno date'' doorCode'' claim'' >>=
            case _ of
              Right recl -> liftEffect do
                self.props.onSuccess recl
                Tracking.reclamation self.props.cusno self.props.subsno date'' (show claim'') "success"
              Left invalidDateInput -> liftEffect do
                self.props.onError invalidDateInput
                Tracking.reclamation self.props.cusno self.props.subsno date'' (show claim'') "error: invalidDateInput"
    submitForm _ Nothing _ = self.setState _ { validationError = Just "Välj ett alternativ." }
    submitForm _ _ _ = Console.error "The entered information is incomplete."

radioButtonOnChange :: Self ->  Maybe String -> Effect Unit
radioButtonOnChange self newClaim = self.setState _ { claim = read =<< newClaim
                                                    , validationError = Nothing
                                                    }


dateInput :: Self -> Maybe Date -> String ->  JSX
dateInput self value label =
  Grid.row
    [ Grid.row_ [ DOM.label_ [ DOM.text label ] ]
    , Grid.row_
        [ DatePicker.datePicker
            { onChange: (_ >>= \newPublicationDate -> self.setState _ { publicationDate = newPublicationDate })
            , className: "delivery-reclamation--date-picker"
            , value: value
            , format: "d.M.yyyy"
            , required: true
            , minDate: Nothing
            , maxDate: self.state.maxPublicationDate
            , disabled : false
            , locale: "sv-FI"
            }
        ]
    ]
    { extraClasses: [ "mitt-konto--date-picker-container" ] }
