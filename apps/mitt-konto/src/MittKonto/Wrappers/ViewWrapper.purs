module MittKonto.Wrappers.ViewWrapper where

import Prelude

import Data.Foldable (foldMap)
import Effect (Effect)
import KSF.AsyncWrapper as AsyncWrapper
import MittKonto.Wrappers.Elements
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Self = React.Self Props State

type Props =
  { content :: JSX
  , wrapperState :: AsyncWrapper.Progress JSX
  , onTryAgain :: Effect Unit
  }

type State = {}

component :: React.Component Props
component = React.createComponent "ViewWrapper"

viewWrapper :: Props -> JSX
viewWrapper = make component { initialState: {}, render }

render :: Self -> JSX
render self@{ props: { content, wrapperState } } =
  AsyncWrapper.asyncWrapper
    { wrapperState: wrapperState
    , readyView: content
    , editingView: identity
    , successView: \msg -> successWrapper msg
    , errorView: \err -> errorWrapper self err
    , loadingView: identity
    }