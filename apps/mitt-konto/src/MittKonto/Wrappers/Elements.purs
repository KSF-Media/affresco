module MittKonto.Wrappers.Elements where

import Prelude

import Data.Foldable (foldMap)
import Effect (Effect)
import KSF.AsyncWrapper as AsyncWrapper
import React.Basic.Classic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

data CloseType = Countdown | XButton | BackArrow
data WrapperType = Basic | Async

successWrapper msg =
  DOM.div { className: "actions-wrapper--action-item"
            , children: [ successContainer [ DOM.div { className: "actions-wrapper--success check-icon" }
                                            , foldMap successMessage msg
                                            ]
                        ]
            }
errorWrapper onTryAgain err =
  DOM.div { className: "actions-wrapper--action-item"
          , children: [ errorContainer [ errorMessage err, tryAgain onTryAgain ] ]
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

tryAgain onTryAgain =
  DOM.span
    { className: "actions-wrapper--try-again"
    , children: [ DOM.text "Försök igen" ]
    , onClick: handler_ onTryAgain
    }

actionsContainer self children =
  DOM.div { className: self.props.containerClass, children }

successContainer children =
  DOM.div { className: "actions-wrapper--success-container flex", children }

errorContainer children =
  DOM.div { className: "actions-wrapper--error-container flex", children }

loadingSpinner = [ DOM.div { className: "tiny-spinner" } ]