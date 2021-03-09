module KSF.DeliveryReclamation where

import Prelude

import Data.Array (filter, snoc, sort)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.HashMap as HashMap
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
import KSF.Api.Package (Product)
import KSF.Grid as Grid
import KSF.InputField as InputField
import KSF.Paper as Paper
import KSF.User as User
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import KSF.Tracking as Tracking

type Self = React.Self Props State

type Props =
  { cusno     :: String
  , onCancel  :: Effect Unit
  , onLoading :: Effect Unit
  , onSuccess :: User.DeliveryReclamation -> Effect Unit
  , onError   :: User.InvalidDateInput -> Effect Unit
  , products  :: HashMap.HashMap String Product
  , subsno    :: Int
  , userUuid  :: User.UUID
  }

type State =
  { claim              :: Maybe User.DeliveryReclamationClaim
  , maxPublicationDate :: Maybe DateTime
  , product            :: Maybe Paper.Paper
  , publicationDate    :: Maybe DateTime
  , validationError    :: Maybe String
  }

deliveryReclamation :: Props -> JSX
deliveryReclamation = make component { initialState, render, didMount }

initialState :: State
initialState =
  { claim: Nothing
  , maxPublicationDate: Nothing
  , product: Nothing
  , publicationDate: Nothing
  , validationError: Nothing
  }

component :: React.Component Props
component = React.createComponent "DeliveryReclamation"

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  self.setState _ { maxPublicationDate = Just now }

render :: Self -> JSX
render self@{ props: { products }, setState, state: { publicationDate, claim, maxPublicationDate }} =
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
              [ productChoice
              , publicationDayInput
              , claimExtensionInput
              , claimNewDeliveryInput
              ] `snoc` foldMap errorMessage self.state.validationError
                `snoc` DOM.div
                        { children: [ submitFormButton ]
                        , className: "mt2 clearfix"
                        }
          }

    productChoice =
      DOM.div_
        [ DOM.div_ [ DOM.label_ [ DOM.text "Choose product" ] ]
        , DOM.div_ $
            productInput <$> (sort <<< filter (\p -> not p.digital) <<< HashMap.toArrayBy (flip const) $ products)
        ]

    productInput product =
      InputField.inputField
        { type_: InputField.Radio
        , placeholder: "Product"
        , name: "product"
        , onChange: onProductChange
        , value: Just product.paper.id
        , label: Just product.paper.name
        , validationError: Nothing
        }

    publicationDayInput = dateInput self.state.publicationDate "Utgivningsdatum"

    claimExtensionInput =
      InputField.inputField
        { type_: InputField.Radio
        , placeholder: "Extension"
        , name: "claim"
        , onChange: onClaimChange
        , value: Just "Extension"
        , label: Just "Jag klarar mig utan den uteblivna tidningen, förläng i stället min prenumeration med en dag"
        , validationError: Nothing
        }

    claimNewDeliveryInput =
      InputField.inputField
        { type_: InputField.Radio
        , placeholder: "New delivery"
        , name: "claim"
        , onChange: onClaimChange
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

    submitForm :: Maybe Paper.Paper -> Maybe DateTime -> Maybe User.DeliveryReclamationClaim -> Effect Unit
    submitForm (Just paper') (Just date') (Just claim') = do
      Aff.launchAff_ do
        createDeliveryReclamation date' claim'
      where
        createDeliveryReclamation :: DateTime -> User.DeliveryReclamationClaim -> Aff Unit
        createDeliveryReclamation date'' claim'' = do
          liftEffect $ self.props.onLoading
          User.createDeliveryReclamation self.props.userUuid self.props.subsno date'' claim'' >>=
            case _ of
              Right recl -> liftEffect do
                self.props.onSuccess recl
                Tracking.reclamation self.props.cusno (show self.props.subsno) date'' (show claim'') "success"
              Left invalidDateInput -> liftEffect do
                self.props.onError invalidDateInput
                Tracking.reclamation self.props.cusno (show self.props.subsno) date'' (show claim'') "error: invalidDateInput"
    submitForm Nothing _ _ = self.setState _ { validationError = Just "Välj en produkt." }
    submitForm _ _ Nothing = self.setState _ { validationError = Just "Välj ett alternativ." }
    submitForm _ _ _ = Console.error "The entered information is incomplete."

    onProductChange :: Maybe String -> Effect Unit
    onProductChange id =
      setState _ { product = read =<< _.id <$> _.paper <$> (flip HashMap.lookup products =<< id)
                , validationError = Nothing
                }

    onClaimChange :: Maybe String -> Effect Unit
    onClaimChange newClaim = self.setState _ { claim = read =<< newClaim
                                             , validationError = Nothing
                                             }


    dateInput :: Maybe DateTime -> String ->  JSX
    dateInput value label =
      Grid.row
        [ Grid.row_ [ DOM.label_ [ DOM.text label ] ]
        , Grid.row_
            [ DatePicker.datePicker
                { onChange: (_ >>= \newPublicationDate -> setState _ { publicationDate = newPublicationDate })
                , className: "delivery-reclamation--date-picker"
                , value: toNullable $ fromDateTime <$> value
                , format: "d.M.yyyy"
                , required: true
                , minDate: toNullable $ Nothing
                , maxDate: toNullable $ fromDateTime <$> maxPublicationDate
                , disabled : false
                , locale: "sv-FI"
                }
            ]
        ]
        $ Just { extraClasses: [ "mb2" ] }
