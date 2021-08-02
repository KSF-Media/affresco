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
            { className: accountClass <>
                if menuVisible then
                  " " <> menuVisibleAccountClass
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
            { className: iconButtonClass <> searchButtonClass <>
                if menuVisible then
                  " " <> menuVisibleSearchButtonClass
                else
                  mempty
            , children: [ DOM.div { className: searchIconClass } 
                        , DOM.div_ [ DOM.text "SÖK" ]]
            , onClick: handler_ do
                setState \s -> s { menuVisible = not menuVisible }
            }
        , DOM.div
            { className: menuButtonClass <>
                if menuVisible then
                  " " <> menuVisibleMenuButtonClass
                else
                  mempty
            , children: [ DOM.div_ [ DOM.text "MENU" ]
                        , DOM.div 
                            { className: menuIconClass <> 
                                if menuVisible then
                                  " " <> menuVisibleMenuIconClass
                                else
                                  mempty
                            } ]
            , onClick: handler_ do
                setState \s -> s { menuVisible = not menuVisible }
            }

        , DOM.div 
            { className: menuOverlayClass <>
                           if menuVisible then
                             " " <> visibleMenuOverlayClass
                           else
                             mempty
            }
        ]
    }
  where
    block = "mosaico-header"

    menuVisibleModifier = "--menu-visible"

    accountElement = "__account"
    accountClass = block <> accountElement
    menuVisibleAccountClass = accountClass <> menuVisibleModifier

    searchModifier = "--search"
    menuModifier = "--menu"

    iconButtonElement = "__icon-button"
    iconButtonClass = block <> iconButtonElement
    searchButtonClass = iconButtonClass <> searchModifier
    menuVisibleSearchButtonClass = iconButtonClass <> searchModifier

    iconElement = "__icon"
    iconClass = block <> iconElement
    searchIconClass = searchIconClass <> menuVisibleModifier

    menuButtonElement = "__menu-button"
    menuButtonClass = block <> menuButtonElement
    menuVisibleMenuButtonClass = menuButtonClass <> menuVisibleModifier

    menuTextElement = "__menu-text"
    menuTextClass = block <> menuTextElement

    menuIconElement = "__menu-icon"
    menuIconClass = block <> menuIconElement
    menuVisibleMenuIconClass = menuIconClass <> menuVisibleModifier

    menuOverlayElement = "__menu-overlay"
    menuOverlayClass = block <> menuOverlayElement
    visibleModifier = "--visible"
    visibleMenuOverlayClass = menuOverlayClass <> visibleModifier

-- The characteristic line at the top of every KSF media's site
topLine :: JSX
topLine = DOM.hr { className: "mosaico-top-line" }

-- The separator between the header and the rest of the page
mainSeparator :: JSX
mainSeparator = DOM.hr { className: "mosaico-main-separator" }
