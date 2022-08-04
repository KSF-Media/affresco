module Mosaico.EpaperBanner where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM


render :: JSX
render =
  let blockClass = "mosaico-epaper__liftup"
  in DOM.section
      { className: blockClass
      , children:
        [ DOM.header_ [ DOM.h2_ [ DOM.text "E-tidningen" ] ]
        , DOM.a
          { href: "/epaper"
          , className: blockClass <> "--container"
          , children:
            [ DOM.span
                { className: blockClass <> "--cover"
                , children: [ DOM.span
                                { className: "sr-only"
                                , children: [DOM.text "E-tidningen"]
                                } ]
             } ]
          }
        ]
      }
