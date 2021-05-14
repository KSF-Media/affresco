module Mosaico.LoginModal where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Set as Set
import Data.Traversable (foldMap)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import KSF.User (UserError, User)
import KSF.User.Login as Login
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as KeyboardEvent

type Props =
  { onUserFetch :: Either UserError User -> Effect Unit
  , onClose :: Effect Unit
  }

type State =
  { loginComponent :: Login.Props -> JSX
  }

loginModal :: Component Props
loginModal = do
  loginComponent <- Login.login
  component "LoginModal" \props -> React.do
    state /\ setState <- useState { loginComponent }

    -- Add eventListener for ESC key, which closes the modal
    -- Remove the listener when the LoginModal is unmounted
    useEffectOnce $ do
      let captureEsc e = guard (e == "Escape") props.onClose
          fromKeyboardEvent = KeyboardEvent.fromEvent >=> (KeyboardEvent.key >>> Just)

      e <- eventListener $ fromKeyboardEvent >>> foldMap captureEsc

      document <- Web.document =<< Web.window
      addEventListener (EventType "keydown") e false (toEventTarget document)
      pure $ removeEventListener (EventType "keydown") e false (toEventTarget document)
    pure $ render props state

render :: Props -> State -> JSX
render props state =
  DOM.div
    { className: "mosaico--login-modal-container"
    , children:
        [ DOM.div
            { className: "mosaico--login-modal"
            , children:
                [ DOM.div
                    { className: "mosaico--login-modal_title"
                    , children:
                        [ DOM.h1_ [ DOM.text "Logga in" ]
                        , DOM.span
                            { className: "mosaico--login-modal_close"
                            , onClick: handler_ props.onClose
                            }
                        ]
                    }
                , state.loginComponent
                    { onMerge: pure unit
                    , onMergeCancelled: pure unit
                    , onRegister: pure unit
                    , onRegisterCancelled: pure unit
                    , onUserFetch: props.onUserFetch
                    , onLogin: \aff -> do
                        Console.log "LAUNCHING AFF"
                        -- TODO: spinners and things
                        Aff.launchAff_ aff
                    , disableSocialLogins: Set.empty
                    }
                ]
            }
        ]
    }
