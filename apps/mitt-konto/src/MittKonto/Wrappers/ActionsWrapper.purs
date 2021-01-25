module MittKonto.Wrappers.ActionsWrapper where

import Prelude

import Effect (Effect)
import KSF.AsyncWrapper as AsyncWrapper
import MittKonto.Wrappers.Elements (errorWrapper, successWrapper)
import React.Basic.Classic (JSX, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM

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
render self@{ props: { actions, wrapperState, onTryAgain } } =
  AsyncWrapper.asyncWrapper
    { wrapperState: wrapperState
    , readyView: actionsContainer actions
    , editingView: identity
    , successView: \msg -> DOM.div_
                             [ actionsContainer actions
                             , successWrapper msg
                             ]
    , errorView: \err -> DOM.div_
                           [ actionsContainer actions
                           , errorWrapper onTryAgain err
                           ]
    , loadingView: identity
    }
  where
    actionsContainer children =
      DOM.div { className: self.props.containerClass, children }