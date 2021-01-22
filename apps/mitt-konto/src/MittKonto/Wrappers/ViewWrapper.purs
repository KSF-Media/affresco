module MittKonto.Wrappers.RouteWrapper where

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

type RouteWrapperState =
  { closeable :: Boolean
  , closeAutomatically :: AutoClose
  , titleText :: String
  , renderedContent :: JSX
  , onClose :: Effect Unit
  }

type SetRouteWrapperState = (RouteWrapperState -> RouteWrapperState) -> Effect Unit

class RouteWrapperContent p where
  instantiate :: p -> SetRouteWrapperState -> Effect Unit

component :: forall p. React.Component (Props p)
component = React.createComponent "RouteWrapper"

routeWrapper :: forall p. (RouteWrapperContent p) => (Props p) -> JSX
routeWrapper = make component
  { initialState
  , didMount
  , render
  }
  where
    initialState :: RouteWrapperState
    initialState =
      { closeable: true
      , closeAutomatically: Off
      , titleText: mempty
      , renderedContent: mempty
      , onClose: pure unit
      }

    didMount :: React.Self (Props p) RouteWrapperState -> Effect Unit
    didMount self@{ props: { content }, setState } = do
      instantiate content setState

    render :: React.Self (Props p) RouteWrapperState -> JSX
    render self@{ props: { closeType, route, routeFrom }, state: { closeable, closeAutomatically, titleText, onClose, renderedContent } } =
      Router.route
        { exact: true
        , path: Just route
        , render: \_ -> DOM.div_
            [ header
            , renderedContent
            ] <> autoClose self.props closeAutomatically
        }
      where
        header :: JSX
        header = Grid.row_
          [ if closeable then
              case closeType of
                Back ->
                  Router.link
                    { to: { pathname: routeFrom, state: {} }
                    , children: [ ]
                    , className: "mitt-konto--backwards"
                    }
                _ -> mempty
            else
              mempty
          , DOM.div
              { className: "col col-11"
              , children: [ title ]
              }
          , if closeable then
              case closeType of
                XButton ->
                  DOM.div
                    { className: "col-1 flex credit-card-choice--close-icon"
                    , children: [ Router.link
                                    { to: { pathname: routeFrom, state: {} }
                                    , children: [ ]
                                    , className: "close-icon"
                                    }
                                ]
                    , onClick: handler_ onClose
                    }
                _ -> mempty
            else
              mempty
          ]

        title :: JSX
        title = DOM.h3_ [ DOM.text titleText ]

autoClose :: forall p. (RouteWrapperContent p) => Props p -> AutoClose -> JSX
autoClose props@{ route, routeFrom } Immediate = Router.redirect
  { to: { pathname: routeFrom
        , state: {}
        }
  , from: route
  , push: true
  }
autoClose props@{ route, routeFrom } (Delayed delay) = Router.delayedRedirect
  { to: { pathname: routeFrom
        , state: {}
        }
  , from: route
  , push: true
  , delay
  }
autoClose _ Off = mempty