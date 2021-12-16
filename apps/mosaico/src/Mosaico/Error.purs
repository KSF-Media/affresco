module Mosaico.Error where

import Prelude

import Lettera.Models (notFoundArticle, fromFullArticle)
import React.Basic (JSX)
import React.Basic.DOM as DOM

somethingWentWrong :: JSX
somethingWentWrong =
  DOM.div
    { className: "mosaico--error-with-aside"
    , children: [ DOM.text "Oj! Något gick fel, ladda om sidan." ]
    }

notFoundWithAside :: JSX
notFoundWithAside =
  DOM.div
    { className: "mosaico--error-with-aside"
    , children: [ DOM.h1_ [ DOM.text $ _.title $ fromFullArticle notFoundArticle ] ]
    }
