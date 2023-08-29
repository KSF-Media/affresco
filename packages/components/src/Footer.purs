module KSF.Footer where

import KSF.Icons (papers)
import React.Basic (JSX)
import React.Basic.DOM as DOM

render :: JSX
render =
  DOM.div
    { className: "footer--container"
    , children:
        [ DOM.div
            { children: [ DOM.hr { className: "footer--break" } ]
            }
        , paperImages
        , ksfAddress
        , ksfLogo
        ]
    }

ksfLogo :: JSX
ksfLogo =
  DOM.div
    { children:
        [ DOM.img
            { className: "footer--ksf-logo"
            , src: papers.ksf
            }
        ]
    }

ksfAddress :: JSX
ksfAddress =
  DOM.div
    { className: "footer--ksf-address"
    , children:
      [ addressText "Hufvudstadsbladet Ab"
      , separator
      , addressText "Mannerheimvägen 18"
      , separator
      , addressText "00100 Helsingfors" ]
    }
  where
    separator = DOM.span { className: "footer--address-separator", children: [ DOM.text "–" ] }
    addressText text = DOM.div_ [ DOM.text text ]

paperImages :: JSX
paperImages =
  DOM.div
    { className: "footer--paper-images"
    , children:
        [ imageLink "https://www.hbl.fi/" papers.hbl
        , imageLink "https://www.vastranyland.fi/" papers.vn
        , imageLink "https://www.ostnyland.fi/" papers.on
        ]
    }
  where
    imageLink href icon =
      DOM.a
        { href
        , children: [ DOM.img { src: icon } ]
        , target: "_blank"
        }
