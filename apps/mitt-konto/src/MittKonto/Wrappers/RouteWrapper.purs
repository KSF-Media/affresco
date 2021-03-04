module MittKonto.Wrappers.RouteWrapper where

import Prelude

import Effect (Effect)
import MittKonto.Wrappers.Elements (AutoClose(..), CloseType(..))
import Prim.Row (class Nub, class Union)
import React.Basic (JSX)
import React.Basic.Hooks (Component, component, useState, (/\)) -- \)
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Router as Router
import Record as Record

type Props p =
  { contentProps :: p
  , closeType :: CloseType
  , route :: String
  , routeFrom :: String
  }

type State =
  { closeable :: Boolean
  , closeAutomatically :: AutoClose
  , titleText :: String
  , onClose :: Effect Unit
  }

type SetRouteWrapperState = (State -> State) -> Effect Unit

type Setter = ( setWrapperState :: SetRouteWrapperState )

routeWrapper :: forall b u p. Union b Setter u => Nub u p => Component (Record p) -> Component (Props (Record b))
routeWrapper wrappedComponent = do
  content <- wrappedComponent
  component "RouteWrapper" \props -> React.do
    state /\ setWrapperState <- useState initialState
    let renderedContent = content $ Record.merge props.contentProps { setWrapperState: setWrapperState }
    pure $ render props state renderedContent
  where
    initialState :: State
    initialState =
      { closeable: true
      , closeAutomatically: Off
      , titleText: mempty
      , onClose: pure unit
      }

    render props@{ closeType, route, routeFrom } { closeable, closeAutomatically, titleText, onClose } renderedContent = DOM.div
         { className: "route-wrapper"
         , children:
             [ header
             , renderedContent
             , autoClose props closeAutomatically
             ]
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

autoClose :: forall p. Props p -> AutoClose -> JSX
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
