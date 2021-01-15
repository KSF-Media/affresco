module MittKonto.Wrappers.ViewWrapper where

import Prelude

import Data.Foldable (foldMap)
import Effect (Effect)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Grid as Grid
import MittKonto.Wrappers.Elements
import React.Basic.Classic (JSX, element, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Router as Router

type Self = forall p. (ViewWrapperContent p) => React.Self (Props p) ViewWrapperState

-- TODO: the `closeType` prop is there because it would be good to control the way we close the view,
-- as now separate views have separate implementations, but they could be unified
-- through this component, which would need expansion.
-- Something to think about.
type Props p =
  { content :: p
  , closeType :: CloseType
  , onTryAgain :: Effect Unit
  }

type ViewWrapperState =
  { closeable :: Boolean
  , asyncWrapperState :: AsyncWrapper.Progress JSX
  , titleText :: String
  , onCancel :: Effect Unit
  }

type SetState = (ViewWrapperState -> ViewWrapperState) -> Effect Unit

class ViewWrapperContent p where
  instantiate :: p -> SetState -> JSX

initialState =
  { closeable: true
  , asyncWrapperState: AsyncWrapper.Ready
  , titleText: mempty
  , onCancel: pure unit
  }

component :: forall p. (ViewWrapperContent p) => React.Component (Props p)
component = React.createComponent "ViewWrapper"

viewWrapper :: forall p. (ViewWrapperContent p) => (Props p) -> JSX
viewWrapper = make component { initialState, render }

render :: forall p. (ViewWrapperContent p) => React.Self (Props p) ViewWrapperState -> JSX
render self@{ props: { content, closeType }, state: { asyncWrapperState, closeable, titleText, onCancel }, setState } =
  DOM.div_ [ header, asyncWrapper ]
  where
    header :: JSX
    header = Grid.row_
      [ DOM.div
          { className: "col col-11"
          , children: [ title ]
          }
      , if closeable then
          DOM.div
            { className: "col-1 flex credit-card-choice--close-icon"
            , children: [ Router.link
                            { to: { pathname: "/", state: {} }
                            , children: [ ]
                            , className: "close-icon"
                            }
                        ]
            , onClick: handler_ onCancel
            }
        else
          mempty
      ]

    title :: JSX
    title = DOM.h3_ [ DOM.text titleText ]

    asyncWrapper :: JSX
    asyncWrapper = AsyncWrapper.asyncWrapper
      { wrapperState: asyncWrapperState
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