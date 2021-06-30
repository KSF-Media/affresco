module Mosaico.Header where

import Prelude

import Effect (Effect)
import Mosaico.Header.Menu as Menu
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

type Self =
  { state :: State
  , setState :: SetState
  }

type State =
  { menuVisible :: Boolean
  , menuComponent :: Menu.Props -> JSX
  }

type SetState = (State -> State) -> Effect Unit

headerComponent :: Component {}
headerComponent = do
  menuComponent <- Menu.menuComponent
  component "Header" \_ -> React.do
    let initialState =
          { menuVisible: false
          , menuComponent
          }
    state /\ setState <- useState initialState
    pure $ render { state, setState }

render :: Self -> JSX
render { state: { menuVisible, menuComponent }, setState } =
  DOM.header
    { className: block
    , children:
        [ DOM.div
            { className: block <> "__left-links"
            , children:
                [ DOM.a_ [ DOM.text "KONTAKTA OSS" ]
                , DOM.text "|"
                , DOM.a_ [ DOM.text "E-TIDNINGEN" ]
                ]
            }
        , DOM.div
            { className: block <> "__right-links"
            , children:
                [ DOM.ul_
                    [ DOM.li_
                        [ DOM.a_ [ DOM.text "KUNDSERVICE" ]
                        ]
                    , DOM.li_
                        [ DOM.a
                            { className: block <> "__prenumerera-link"
                            , children: [ DOM.text "PRENUMERERA" ]
                            }
                        ]
                    ]
                ]
            }
        , menuComponent { visible: menuVisible }
        , DOM.div
            { className: block <> "__logo"
            }
        , DOM.div
            { className: block <> "__account" <>
                if menuVisible then
                  " " <> block <> "__account" <> "--menu-visible"
                else
                  mempty
            , children: [ DOM.text "NAME"]
            }
        , DOM.nav
            { className: block <> "__center-links"
            , children:
                [ DOM.a_ [ DOM.text "OPINION" ]
                , DOM.a_ [ DOM.text "KULTUR" ]
                , DOM.a_ [ DOM.text "SPORT" ]
                , DOM.a_ [ DOM.text "ANNAT" ]
                ]
            }
        , DOM.div
            { className: block <> "__menu-button" <>
                if menuVisible then
                  " " <> block <> "__menu-button" <> "--menu-visible"
                else
                  mempty
            , children: [ DOM.text "MENU"
                        , DOM.div { className: block <> "__menu-icon" } ]
            , onClick: handler_ do
                setState \s -> s { menuVisible = not menuVisible }
            }
        ]
    }
  where
    block = "mosaico-header"
