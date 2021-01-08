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

type Self = forall p. (ViewWrapperContent p) => React.Self (Props p) State

-- TODO: the `closeType` prop is there because it would be good to control the way we close the view,
-- as now separate views have separate implementations, but they could be unified
-- through this component, which would need expansion.
-- Something to think about.
type Props p =
  { content :: p
  , closeType :: CloseType
  , onTryAgain :: Effect Unit
  }

type State =
  { closeable :: Boolean
  , wrapperState :: AsyncWrapper.Progress JSX
  }

type SetState = (State -> State) -> Effect Unit

class ViewWrapperContent p where
  instantiate :: p -> SetState -> JSX

initialState =
  { closeable: true
  , wrapperState : AsyncWrapper.Ready
  }

component :: forall p. (ViewWrapperContent p) => React.Component (Props p)
component = React.createComponent "ViewWrapper"

viewWrapper :: forall p. (ViewWrapperContent p) => (Props p) -> JSX
viewWrapper = make component { initialState, render }

render :: forall p. (ViewWrapperContent p) => React.Self (Props p) State -> JSX
render self@{ props: { content, closeType }, state: { wrapperState }, setState } =
  AsyncWrapper.asyncWrapper
    { wrapperState: wrapperState
    , readyView: instantiate content setState
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