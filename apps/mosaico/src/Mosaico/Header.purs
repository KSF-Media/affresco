module Mosaico.Header where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM

header :: JSX
header =
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
        , DOM.div
            { className: block <> "__logo"
            }
        , DOM.div
            { className: block <> "__account"
            , children: [ DOM.text "NAME"]
            }
        , DOM.nav
            { className: block <> "__menu-links"
            , children:
                [ DOM.a_ [ DOM.text "OPINION" ]
                , DOM.a_ [ DOM.text "KULTUR" ]
                , DOM.a_ [ DOM.text "SPORT" ]
                , DOM.a_ [ DOM.text "ANNAT" ]
                ]
            }
        , DOM.div
            { className: block <> "__menu-button"
            , children: [ DOM.text "MENU"]
            }
        ]
    }
  where
    block = "mosaico-header"