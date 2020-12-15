module KSF.Subscription.Cancel where

import Prelude

import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Read (read)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Grid as Grid
import KSF.InputField as InputField
import KSF.User as User
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import KSF.Tracking as Tracking


type State =
  { reason          :: Maybe User.CancelReason
  , notes           :: Maybe String
  , validationError :: Maybe String
  }

type Self = React.Self Props State

type Props =
  { subsno    :: Int
  , cusno     :: String
  , userUuid  :: User.UUID
  , onCancel  :: Effect Unit
  , onLoading :: Effect Unit
  , onSuccess :: User.Subscription -> Effect Unit
  , onError   :: String -> Effect Unit
  }

cancelSubscription :: Props -> JSX
cancelSubscription = make component { initialState, render, didMount }

initialState :: State
initialState =
  { reason: Nothing
  , notes: Nothing
  , validationError: Nothing
  }

component :: React.Component Props
component = React.createComponent "CancelSubscription"

didMount :: Self -> Effect Unit
didMount self = do
  pure unit

render :: Self -> JSX
render self@{ state: { reason, notes }} =
  DOM.div
    { className: "clearfix cancel-subscription--container"
    , children:
        [ Grid.row_
           [ DOM.div
               { className: "col col-11"
               , children: [ DOM.h3_ [ DOM.text "Avsluta prenumerationen" ] ]
               }
           , DOM.div
               { className: "col-1 flex cancel-subscription--close-icon"
               , children: [ DOM.div { className: "close-icon" } ]
               , onClick: handler_ self.props.onCancel
               }
           , cancelSubscriptionForm
           ]
        ]
    }
  where
    cancelSubscriptionForm =
      DOM.form
          { onSubmit: handler preventDefault (\_ -> submitForm reason)
          , children:
              [ priceInput
              , distributionInput
              , contentInput
              , changeProductInput
              , otherInput
              , notesInput
              ] `snoc` foldMap errorMessage self.state.validationError
                `snoc` DOM.div
                        { children: [ submitFormButton ]
                        , className: "mt2 clearfix"
                        }
          }

    priceInput =
      InputField.inputField
        { type_: InputField.Radio
        , placeholder: "Pris"
        , name: "reason"
        , onChange: radioButtonOnChange self
        , value: Just "Price"
        , label: Just "Priser är för dyrt"
        , validationError: Nothing
        }

    distributionInput =
      InputField.inputField
        { type_: InputField.Radio
        , placeholder: "Distribution"
        , name: "reason"
        , onChange: radioButtonOnChange self
        , value: Just "Distribution"
        , label: Just "Problem med distributionen"
        , validationError: Nothing
        }

    contentInput =
      InputField.inputField
        { type_: InputField.Radio
        , placeholder: "Content"
        , name: "reason"
        , onChange: radioButtonOnChange self
        , value: Just "Content"
        , label: Just "Innehållet intresserar inte"
        , validationError: Nothing
        }

    otherInput =
      InputField.inputField
         { type_: InputField.Radio
         , placeholder: "Annat"
         , name: "reason"
         , onChange: radioButtonOnChange self
         , value: Just "Other"
         , label: Just "Annat"
         , validationError: Nothing
         }

    changeProductInput =
      InputField.inputField
        { type_: InputField.Radio
        , placeholder: "Ändra product"
        , name: "reason"
        , onChange: radioButtonOnChange self
        , value: Just "ChangeProduct"
        , label: Just "Bytte till en annan produkt"
        , validationError: Nothing
        }

    notesInput =
      InputField.inputField
        { type_: InputField.Text
        , placeholder: "notes TODO"
        , name: "notes"
        , onChange: \newNotes -> self.setState _ { notes = newNotes }
        , value: notes
        , label: Just "notes TODO"
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

    submitForm :: Maybe User.CancelReason -> Effect Unit
    submitForm (Just reason') = do
      Aff.launchAff_ do
        cancel reason'
      where
        cancel :: User.CancelReason -> Aff Unit
        cancel reason'' = do
          liftEffect $ self.props.onLoading
          User.cancelSubscription self.props.userUuid self.props.subsno reason'' notes >>=
            case _ of
              Right canc -> liftEffect do
                self.props.onSuccess canc
                Tracking.cancelSubscription self.props.cusno (show self.props.subsno) (show reason'') (fromMaybe "" notes) "success"
              Left err -> liftEffect do
                self.props.onError err
                Tracking.cancelSubscription self.props.cusno (show self.props.subsno) (show reason'') (fromMaybe "" notes) "error"
    submitForm Nothing = self.setState _ { validationError = Just "Välj ett alternativ." }

radioButtonOnChange :: Self ->  Maybe String -> Effect Unit
radioButtonOnChange self newReason = self.setState _ { reason = read =<< newReason
                                                     , validationError = Nothing
                                                     }
