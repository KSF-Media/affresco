module MittKonto.Wrappers.RouteWrapper where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import MittKonto.Wrappers.Elements (AutoClose(..), CloseType(..))
import React.Basic (JSX)
import React.Basic.Hooks ((/\), Component, component, useState, useEffect)
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Router as Router

type Props p =
  { content :: p
  , closeType :: CloseType
  , route :: String
  , routeFrom :: String
  }

type State =
  { closeable :: Boolean
  , closeAutomatically :: AutoClose
  , titleText :: String
  , renderedContent :: JSX
  , onClose :: Effect Unit
  }

type SetRouteWrapperState = (State -> State) -> Effect Unit

class RouteWrapperContent p where
  instantiate :: p -> SetRouteWrapperState -> Effect Unit

routeWrapper :: forall p t. Eq t => RouteWrapperContent p => (p -> t) -> Component (Props p)
routeWrapper f = do
  component "RouteWrapper" \props@{ content, closeType, route, routeFrom } -> React.do
    state /\ setState <- useState initialState
    useEffect (f content) do
      instantiate content setState
      pure $ pure unit
    pure $ render props state
  where
    initialState :: State
    initialState =
      { closeable: true
      , closeAutomatically: Off
      , titleText: mempty
      , renderedContent: mempty
      , onClose: pure unit
      }

    render :: (Props p) -> State -> JSX
    render props@{ closeType, route, routeFrom } { closeable, closeAutomatically, titleText, onClose, renderedContent } =
      Router.route
        { exact: true
        , path: Just route
        , render: \_ -> DOM.div
            { className: "route-wrapper"
            , children:
                [ header
                , renderedContent
                , autoClose props closeAutomatically
                ]
            }
        }
      where
        header :: JSX
        header = DOM.div $
          case closeType of
            XButton ->
              { className: "header-x-button"
              , children:
                  [ title
                  , if closeable then
                      DOM.div
                        { className: "flex close-button"
                        , children: [ closeLink "close-icon" ]
                        , onClick: handler_ onClose
                        }
                    else mempty
                  ]
              }
            Back ->
              { className: ""
              , children:
                  [ if closeable then
                      DOM.div
                        { children: [ closeLink "mitt-konto--backwards" ]
                        , onClick: handler_ onClose
                        }
                    else
                      mempty
                    , title
                  ]
              }

        title :: JSX
        title = DOM.h3_ [ DOM.text titleText ]

        closeLink :: String -> JSX
        closeLink className =
          Router.link
            { to: { pathname: routeFrom, state: {} }
            , children: [ ]
            , className
            }

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
