module MittKonto.Wrappers.RouteWrapper where

import Prelude

import Effect (Effect)
import MittKonto.Wrappers.Elements (AutoClose(..), CloseType(..))
import React.Basic.Classic (JSX, make)
import React.Basic.Classic as React
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

component :: forall p. React.Component (Props p)
component = React.createComponent "RouteWrapper"

routeWrapper :: forall p. (RouteWrapperContent p) => (Props p) -> JSX
routeWrapper = make component
  { initialState
  , didMount
  , render
  }
  where
    initialState :: State
    initialState =
      { closeable: true
      , closeAutomatically: Off
      , titleText: mempty
      , renderedContent: mempty
      , onClose: pure unit
      }

    didMount :: React.Self (Props p) State -> Effect Unit
    didMount self@{ props: { content }, setState } = do
      instantiate content setState

    render :: React.Self (Props p) State -> JSX
    render self@{ props: { closeType, route, routeFrom }, state: { closeable, closeAutomatically, titleText, onClose, renderedContent } } = DOM.div
         { className: "route-wrapper"
         , children:
             [ header
             , renderedContent
             , autoClose self.props closeAutomatically
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
