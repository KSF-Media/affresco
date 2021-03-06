module KSF.DeliveryReclamation where

import Prelude

import Data.Array (snoc)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String.Read (read)
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
  , claim              :: Maybe User.DeliveryReclamationClaim
  , maxPublicationDate :: Maybe Date
  , validationError    :: Maybe String
  }

type Self = React.Self Props State

type Props =
  { subsno    :: Subsno
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
  , claim: Nothing
  , maxPublicationDate: Nothing
  , validationError: Nothing
  }

component :: React.Component Props
component = React.createComponent "DeliveryReclamation"

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDate
  self.setState _ { maxPublicationDate = Just now }

render :: Self -> JSX
render self@{ state: { publicationDate, claim }} =
  DOM.div
    { className: "clearfix delivery-reclamation--container"
    , children:
        [ Grid.row_
           [ DOM.div
               { className: "col col-11"
               , children: [ DOM.h3_ [ DOM.text "Reklamation" ] ]
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
          { onSubmit: handler preventDefault (\_ -> submitForm publicationDate claim)
          , children:
              [ publicationDayInput
              , claimExtensionInput
              , claimNewDeliveryInput
              ] `snoc`foldMap errorMessage self.state.validationError
                `snoc` DOM.div
                        { children: [ submitFormButton ]
                        , className: "mt2 clearfix"
                        }
          }

    publicationDayInput = dateInput self self.state.publicationDate "Utgivningsdatum"

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

    errorMessage =
      InputField.errorMessage

    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Skicka" ]
          , className: "button-green"
          }

    submitForm :: Maybe Date -> Maybe User.DeliveryReclamationClaim -> Effect Unit
    submitForm (Just date') (Just claim') = do
      Aff.launchAff_ do
        createDeliveryReclamation date' claim'
      where
        createDeliveryReclamation :: Date -> User.DeliveryReclamationClaim -> Aff Unit
        createDeliveryReclamation date'' claim'' = do
          liftEffect $ self.props.onLoading
          User.createDeliveryReclamation self.props.userUuid self.props.subsno date'' claim'' >>=
            case _ of
              Right recl -> liftEffect do
                self.props.onSuccess recl
                Tracking.reclamation self.props.cusno self.props.subsno date'' (show claim'') "success"
              Left invalidDateInput -> liftEffect do
                self.props.onError invalidDateInput
                Tracking.reclamation self.props.cusno self.props.subsno date'' (show claim'') "error: invalidDateInput"
    submitForm _ Nothing = self.setState _ { validationError = Just "Välj ett alternativ." }
    submitForm _ _ = Console.error "The entered information is incomplete."

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
    { extraClasses: [ "mb2" ] }
