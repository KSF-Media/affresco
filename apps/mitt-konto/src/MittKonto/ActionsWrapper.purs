module MittKonto.ActionsWrapper where

import Prelude

import Data.Array (filter)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, toDateTime)
import Data.List (fromFoldable, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.String (trim)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Now as Now
import KSF.Api.Subscription (SubscriptionPaymentMethod(..))
import KSF.AsyncWrapper as AsyncWrapper
import KSF.DescriptionList.Component as DescriptionList
import KSF.Grid as Grid
import KSF.JSError as Error
import KSF.Sentry as Sentry
import KSF.User (User, InvalidDateInput(..))
import KSF.User as User
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import Tracking as Tracking

type Self = React.Self Props State

type Props =
  { actions :: Array JSX
  , wrapperState :: AsyncWrapper.Progress JSX
  , onTryAgain :: Effect Unit
  , containerClass :: String
  }

type State = {}

component :: React.Component Props
component = React.createComponent "ActionsWrapper"

actionsWrapper :: Props -> JSX
actionsWrapper = make component { initialState: {}, render }

render :: Self -> JSX
render self = 
  AsyncWrapper.asyncWrapper
    { wrapperState: self.props.wrapperState
    , readyView: actionsContainer $ self.props.actions
    , editingView: identity
    , successView: \msg -> DOM.div_ 
                             [ actionsContainer self.props.actions
                             , successWrapper msg 
                             ]
    , errorView: \err -> DOM.div_ 
                           [ actionsContainer self.props.actions
                           , errorWrapper err
                           ]
    , loadingView: identity
    }
  where
    successWrapper msg =
      DOM.div { className: "actions-wrapper--action-item"
              , children: [ successContainer [ DOM.div { className: "actions-wrapper--success check-icon" }
                                             , foldMap successMessage msg
                                             ]
                          ]
              }
    errorWrapper err =
      DOM.div { className: "actions-wrapper--action-item"
              , children: [ errorContainer [ errorMessage err, tryAgain ] ]
              }
    
    successMessage msg =
      DOM.div
        { className: "success-text"
        , children: [ DOM.text msg ]
        }
    
    errorMessage msg =
      DOM.div
        { className: "error-text"
        , children: [ DOM.text msg ]
        }
    
    tryAgain =
      DOM.span
        { className: "actions-wrapper--try-again"
        , children: [ DOM.text "Försök igen" ]
        , onClick: handler_ self.props.onTryAgain
        }
    
    actionsContainer children =
      DOM.div { className: self.props.containerClass, children }
    
    successContainer children =
      DOM.div { className: "actions-wrapper--success-container flex", children }
    
    errorContainer children =
      DOM.div { className: "actions-wrapper--error-container flex", children }
    
    loadingSpinner = [ DOM.div { className: "tiny-spinner" } ]