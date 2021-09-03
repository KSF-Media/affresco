module Mosaico.Header where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Mosaico.Header.Menu as Menu
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import Routing.PushState (PushStateInterface)
import Simple.JSON (write)

type Props = { router :: Maybe PushStateInterface }

type Self =
  { state :: State
  , setState :: SetState
  , props :: Props
  }

type State =
  { menuVisible :: Boolean
  , menuComponent :: Menu.Props -> JSX
  }

type SetState = (State -> State) -> Effect Unit

headerComponent :: Component Props
headerComponent = do
  menuComponent <- Menu.menuComponent
  component "Header" \props -> React.do
    let initialState =
          { menuVisible: false
          , menuComponent
          }
    state /\ setState <- useState initialState
    pure $ render { state, setState, props }

render :: Self -> JSX
render { state: { menuVisible, menuComponent }, setState, props } =
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
            , onClick: handler_ $ maybe (pure unit) (\r -> r.pushState (write {}) $ "/") props.router
            }
        , DOM.div
            { className: accountClass <>
                if menuVisible then
                  " " <> menuVisibleAccountClass
                else
                  mempty
            , children: [ DOM.text "NAME" ]
            }
        , DOM.nav
            { className: block <> "__center-links"
            , children:
                if menuVisible then
                  [ searchButton ]
                else
                  [ DOM.a_ [ DOM.text "OPINION" ]
                  , DOM.a_ [ DOM.text "KULTUR" ]
                  , DOM.a_ [ DOM.text "SPORT" ]
                  , DOM.a_ [ DOM.text "ANNAT" ]
                  ]
            }

        , DOM.div
            { className: block <> "__right-buttons"
            , children:
                (if menuVisible then
                   mempty
                 else
                   [ searchButton ])
                <> [ DOM.div
                       { className: iconButtonClass <> " " <> menuButtonClass <>
                           if menuVisible then
                           " " <> menuVisibleIconButtonClass
                           else
                           mempty
                       , children: [ DOM.div_ [ DOM.text "MENU" ]
                                   , DOM.div
                                       { className: iconClass <> " " <> menuIconClass <>
                                           if menuVisible then
                                           " " <> menuVisibleIconClass
                                           else
                                           mempty
                                       } ]
                       , onClick: handler_ do
                           setState \s -> s { menuVisible = not menuVisible }
                       }
                   ]
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

    searchButton :: JSX
    searchButton = DOM.div
                    { className: iconButtonClass <> " " <> searchButtonClass <>
                                " grid-row-3 grid-col-2" <>
                        if menuVisible then
                          " " <> menuVisibleIconButtonClass
                        else
                          mempty
                    , children: [ DOM.div_ [ DOM.text "SÖK" ]
                                , DOM.div { className: iconClass <> " " <> searchIconClass }
                                ]
                    }

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
    menuButtonClass = iconButtonClass <> menuModifier
    menuVisibleIconButtonClass = iconButtonClass <> menuVisibleModifier

    iconElement = "__icon"
    iconClass = block <> iconElement
    searchIconClass = iconClass <> searchModifier
    menuIconClass = iconClass <> menuModifier
    menuVisibleIconClass = iconClass <> menuVisibleModifier

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
