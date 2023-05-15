module Ospedale.Page.Error where

import Prelude

import Effect (Effect)
import Lettera.Fallback as Lettera
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)

renderError :: Effect Unit -> String -> Lettera.FallbackError -> JSX
renderError _  _ (Lettera.FallbackError errorText) =
  DOM.div
    { className: "error-text"
    , children: [ DOM.text errorText ]
    }
renderError relogin reloginMsg Lettera.PermissionDenied =
  DOM.div
    { className: "error-text"
    , children:
        [ DOM.text reloginMsg
        , DOM.button
            { children: [ DOM.text "Logga in" ]
            , onClick: capture_ relogin
            }
        ]
    }
