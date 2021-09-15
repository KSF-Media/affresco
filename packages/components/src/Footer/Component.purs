module KSF.Footer.Component where

import KSF.Icons (papers)
import React.Basic (JSX)
import React.Basic.Classic (createComponent, make)
import React.Basic.Classic as React
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
