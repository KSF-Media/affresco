module Mosaico.Header where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  , props :: Props
  }

type Props = {}

type State = {}

type SetState = (State -> State) -> Effect Unit

headerComponent :: Component Props
headerComponent = do
  component "Header" \props -> React.do
    let initialState = {}
    state /\ setState <- useState initialState

    useEffectOnce do
      pure mempty

    pure $ render { state, setState, props }

render :: Self -> JSX
render { props, state, setState } =
  DOM.header
    { className: "header"
    , children:
        [ DOM.div
            { className: "left-links"
            , children:
                [ DOM.a_ [ DOM.text "KONTAKTA OSS" ]
                , DOM.text " | "
                , DOM.a_ [ DOM.text "E-TIDNINGEN" ]
                ]
            }
        , DOM.div
            { className: "right-links"
            , children:
                [ DOM.ul_
                    [ DOM.li_
                        [ DOM.a_ [ DOM.text "KUNDSERVICE" ]
                        ]
                    , DOM.li_
                        [ DOM.a
                            { className: "prenumerera-link"
                            , children: [ DOM.text "PRENUMERERA" ]
                            }
                        ]
                    ]
                ]
            }
        , DOM.div
            { className: "logo"
            }
        , DOM.div
            { className: "account"
            , children: [ DOM.text "NAME"]
            }
        , DOM.div
            { className: "menu-links"
            , children:
                [ DOM.a_ [ DOM.text "OPINION"]
                , DOM.a_ [ DOM.text "KULTUR"]
                , DOM.a_ [ DOM.text "SPORT"]
                , DOM.a_ [ DOM.text "ANNAT"]
                ]
            }
        , DOM.div
            { className: "menu-button"
            , children: [ DOM.text "MENU"]
            }
        ]
    }