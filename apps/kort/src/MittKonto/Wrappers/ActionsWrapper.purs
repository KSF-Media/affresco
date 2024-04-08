module MittKonto.Wrappers.ActionsWrapper where

import Prelude

import Data.Maybe (fromMaybe)
import Effect (Effect)
import KSF.AsyncWrapper as AsyncWrapper
import MittKonto.Wrappers.Elements (errorWrapper)
import React.Basic (JSX)
import React.Basic.DOM as DOM

type Props =
  { actions :: Array JSX
  , wrapperState :: AsyncWrapper.Progress JSX
  , onTryAgain :: Effect Unit
  , containerClass :: String
  }

render :: Props -> JSX
render { actions, wrapperState, onTryAgain, containerClass } =
  AsyncWrapper.asyncWrapper
    { wrapperState: wrapperState
    , readyView: actionsContainer actions
    , editingView: identity
    , successView: \msg -> DOM.div_
                             [ actionsContainer actions
                             , fromMaybe mempty msg
                             ]
    , errorView: \err -> DOM.div_
                           [ actionsContainer actions
                           , errorWrapper onTryAgain err
                           ]
    , loadingView: identity
    }
  where
    actionsContainer children =
      DOM.div { className: containerClass, children }
