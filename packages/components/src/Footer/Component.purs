module KSF.Footer.Component where

import KSF.Icons (papers)
import React.Basic (JSX, createComponent, make)
import React.Basic as React
import React.Basic.DOM as DOM

type Self = React.Self Props {}
type Props = {}

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = createComponent "Footer"

reactComponent :: React.ReactComponent {}
reactComponent =
  React.toReactComponent (\_ -> {}) component { render, initialState: {} }

footer :: Props -> JSX
footer = make component
  { initialState: {}
  , render
  }

render :: Self -> JSX
render _ =
  DOM.div
    { className: "clearfix mx-auto center mt4 mb4"
    , children:
        [ DOM.div
            { className: "col-10 lg-col-7 mx-auto"
            , children: [ DOM.hr { className: "footer--break" } ]
            }
        , paperImages
        , ksfAddress
        , ksfLogo
        ]
    }

ksfLogo :: JSX
ksfLogo =
  DOM.div
    { className: "col col-12 mt3"
    , children:
        [ DOM.img
            { className: "footer--ksf-logo"
            , src: papers.ksf
            }
        ]
    }

ksfAddress :: JSX
ksfAddress =
  DOM.div
    { className: "footer--ksf-address flex justify-center col col-12 mt3"
    , children:
      [ addressText "KSF Media"
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
    { className: "footer--paper-images flex justify-center col col-12 mt3"
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
