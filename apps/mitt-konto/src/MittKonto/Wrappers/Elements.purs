module MittKonto.Wrappers.Elements where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

data CloseType = XButton | Back
data AutoClose = Immediate | Delayed Number | Off

derive instance eqAutoClose :: Eq AutoClose

successWrapper :: Maybe JSX -> String -> JSX
successWrapper extra msg =
  DOM.div
    { className: "actions-wrapper--action-item"
    , children:
        [ successContainer
            [ DOM.div { className: "actions-wrapper--success check-icon" }
            , successMessage msg
            ]
        ]
    } <> fromMaybe mempty extra

errorWrapper :: Effect Unit -> String -> JSX
errorWrapper onTryAgain err =
  DOM.div { className: "actions-wrapper--action-item"
          , children: [ errorContainer [ errorMessage err, tryAgain onTryAgain ] ]
          }

successMessage :: String -> JSX
successMessage msg =
  DOM.div
    { className: "success-text"
    , children: [ DOM.text msg ]
    }

errorMessage :: String -> JSX
errorMessage msg =
  DOM.div
    { className: "error-text"
    , children: [ DOM.text msg ]
    }

tryAgain :: Effect Unit -> JSX
tryAgain onTryAgain =
  DOM.span
    { className: "actions-wrapper--try-again"
    , children: [ DOM.text "Försök igen" ]
    , onClick: handler_ onTryAgain
    }

successContainer :: Array JSX -> JSX
successContainer children =
  DOM.div { className: "actions-wrapper--success-container flex", children }

errorContainer :: Array JSX -> JSX
errorContainer children =
  DOM.div { className: "actions-wrapper--error-container flex", children }

loadingSpinner :: Array JSX
loadingSpinner = [ DOM.div { className: "tiny-spinner" } ]
