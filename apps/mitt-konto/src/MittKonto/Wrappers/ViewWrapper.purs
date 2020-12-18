module MittKonto.Wrappers.ViewWrapper where

import Prelude

import Data.Foldable (foldMap)
import Effect (Effect)
import KSF.AsyncWrapper as AsyncWrapper
import MittKonto.Wrappers.Elements
import React.Basic.Classic (JSX, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Router as Router

type Self = React.Self Props State

-- TODO: the `closeType` prop is there because it would be good to control the way we close the view, 
-- as now separate views have separate implementations, but they could be unified 
-- through this component, which would need expansion.
-- Something to think about.
type Props =
  { content :: JSX
  , wrapperState :: AsyncWrapper.Progress JSX
  , closeType :: CloseType
  , onTryAgain :: Effect Unit
  }

type State = {}

component :: React.Component Props
component = React.createComponent "ViewWrapper"

viewWrapper :: Props -> JSX
viewWrapper = make component { initialState: {}, render }

render :: Self -> JSX
render self@{ props: { content, closeType, wrapperState } } =
  AsyncWrapper.asyncWrapper
    { wrapperState: wrapperState
    , readyView: content
    , editingView: identity
    , successView: \msg -> close closeType <> successWrapper msg
    , errorView: \err -> errorWrapper self err
    , loadingView: identity
    }

close :: CloseType -> JSX
close Countdown = Router.delayedRedirect
  { to: { pathname: "/"
        , state: {}
        }
  , from: "/kortt/uppdatera"
  , push: true
  , delay: 2000.0
  }
close _ = mempty