module MittKonto.Wrappers.RouteWrapper where

import Prelude

import Data.Maybe (isNothing)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AVar (empty, tryPut, tryTake) as AVar
import Effect.Aff.AVar (tryRead) as AVar
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import MittKonto.Wrappers.Elements (AutoClose(..), CloseType(..))
import Prim.Row (class Nub, class Union)
import React.Basic (JSX)
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, (/\)) -- \)
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import Routing.PushState (PushStateInterface)
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

routeWrapper :: forall b u p. Union b Setter u => Nub u p => PushStateInterface -> Component (Record p) -> Component (Props (Record b))
routeWrapper router wrappedComponent = do
  closed <- AVar.empty
  content <- wrappedComponent
  component "RouteWrapper" \props -> React.do
    state /\ setWrapperState <- useState initialState
    useEffectOnce do
      _ <- AVar.tryTake closed
      pure do
        _ <- AVar.tryPut unit closed
        pure unit
    useEffect state.closeAutomatically do
      let close = router.pushState (unsafeToForeign {}) props.routeFrom
      case state.closeAutomatically of
        Immediate -> close
        Delayed ms -> launchAff_ do
          delay $ Milliseconds ms
          cl <- AVar.tryRead closed
          when (isNothing cl) $ liftEffect close
        Off -> pure unit
      pure $ pure unit
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

    render { closeType, routeFrom } { closeable, titleText, onClose } renderedContent = DOM.div
         { className: "route-wrapper"
         , children:
             [ header
             , renderedContent
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
        title = if titleText == "" then mempty else DOM.h3_ [ DOM.text titleText ]

        closeLink :: String -> JSX
        closeLink className =
          DOM.div
            { className
            , onClick: handler preventDefault $ const $ router.pushState (unsafeToForeign {}) routeFrom
            }
