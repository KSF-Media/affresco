module Mosaico.Error where

import React.Basic (JSX)
import React.Basic.DOM as DOM

somethingWentWrong :: JSX
somethingWentWrong =
  DOM.div
    { className: "mosaico--static-page-error"
    , children: [ DOM.text "Oj! Något gick fel, ladda om sidan." ]
    }