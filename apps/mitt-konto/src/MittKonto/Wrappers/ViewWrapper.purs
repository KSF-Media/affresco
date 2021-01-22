module MittKonto.Wrappers.ViewWrapper where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Grid as Grid
import MittKonto.Wrappers.Elements
import React.Basic.Classic (JSX, element, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Router as Router
import Record as Record

type Props p =
  { content :: p
  , closeType :: CloseType
  , route :: String
  , routeFrom :: String
  , wrapperType :: WrapperType
  }

type ViewWrapperState =
  { closeable :: Boolean
  , closeAutomatically :: AutoClose
  , titleText :: String
  , renderedContent :: JSX
  , onCancel :: Effect Unit
  }

type SetViewWrapperState = (ViewWrapperState -> ViewWrapperState) -> Effect Unit

class ViewWrapperContent p where
  instantiate :: p -> SetViewWrapperState -> Effect Unit

component :: forall p. React.Component (Props p)
component = React.createComponent "ViewWrapper"

viewWrapper :: forall p. (ViewWrapperContent p) => (Props p) -> JSX
viewWrapper = make component
  { initialState
  , didMount
  , render
  }
  where
    initialState :: ViewWrapperState
    initialState =
      { closeable: true
      , closeAutomatically: Off
      , titleText: mempty
      , renderedContent: mempty
      , onCancel: pure unit
      }

    didMount :: React.Self (Props p) ViewWrapperState -> Effect Unit
    didMount self@{ props: { content }, setState } = do
      instantiate content setState

    render :: React.Self (Props p) ViewWrapperState -> JSX
    render self@{ props: { route, routeFrom }, state: { closeable, closeAutomatically, titleText, onCancel, renderedContent } } =
      Router.route
        { exact: true
        , path: Just route
        , render: \_ -> DOM.div_
            [ header
            , renderedContent
            ] <> case closeAutomatically of
                  On delay -> autoClose self.props delay
                  Off      -> mempty
        }
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
                                { to: { pathname: routeFrom, state: {} }
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

autoClose :: forall p. (ViewWrapperContent p) => Props p -> Number -> JSX
autoClose props@{ route, routeFrom } delay = Router.delayedRedirect
  { to: { pathname: routeFrom
        , state: {}
        }
  , from: route
  , push: true
  , delay
  }