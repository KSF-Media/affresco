module KSF.DeliveryReclamation where

import Prelude (Unit, bind, discard, ($), (<$>), (>>=))

import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.JSDate (fromDateTime)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.String.Read (read)
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
  , onSuccess :: Persona.DeliveryReclamation -> Effect Unit
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
              , DOM.div
                  { children: [ submitFormButton ]
                  , className: "mt2 clearfix"
                  }
              ]
          }

    publicationDayInput = dateInput self self.state.publicationDate "Utgivningsdatum"

    claimExtensionInput =
      InputField.inputField
        { type_: InputField.Radio
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
        , value: Just "Extension"
        , label: "Jag vill förlänga prenumerationen"
        , validationError: Nothing
        }

    claimNewDeliveryInput =
      InputField.inputField
        { type_: InputField.Radio
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
        , value: Just "NewDelivery"
        , label: "Jag vill få tidningen"
        , validationError: Nothing
        }

    submitFormButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Skicka" ]
          , className: "button-green"
          }

    submitForm :: Maybe DateTime -> Maybe Persona.DeliveryReclamationClaim -> Effect Unit
    submitForm (Just date') (Just claim') = do
      Aff.launchAff_ do
        createDeliveryReclamation date' claim'
      where
        createDeliveryReclamation :: DateTime -> Persona.DeliveryReclamationClaim -> Aff Unit
        createDeliveryReclamation date'' claim'' = do
          liftEffect $ self.props.onLoading
          User.createDeliveryReclamation self.props.userUuid self.props.subsno date'' claim'' >>=
            case _ of
              Right recl -> liftEffect $ self.props.onSuccess recl
              Left invalidDateInput -> liftEffect $ self.props.onError invalidDateInput
    submitForm _ _ = Console.error "The entered information is incomplete."

dateInput :: Self -> Maybe DateTime -> String ->  JSX
dateInput self value label =
  Grid.row
    [ Grid.row_ [ DOM.label_ [ DOM.text label ] ]
    , Grid.row_
        [ DatePicker.datePicker
            { onChange: (_ >>= \newPublicationDate -> self.setState _ { publicationDate = newPublicationDate })
            , className: "delivery-reclamation--date-picker"
            , value: toNullable $ fromDateTime <$> value
            , format: "d.M.yyyy"
            , required: true
            , minDate: toNullable $ Nothing
            , maxDate: toNullable $ fromDateTime <$> self.state.maxPublicationDate
            , disabled : false
            , locale: "sv-FI"
            }
        ]
    ]
    $ Just { extraClasses: [ "mb2" ] }
