module MittKonto.Main.Elements where

import KSF.Spinner as Spinner
import MittKonto.Main.Types as Types
import React.Basic (JSX)
import React.Basic.DOM as DOM

break :: JSX
break = DOM.hr { className: "mitt-konto--break" }

-- | This break will appear only in narrower views
disappearingBreak :: JSX
disappearingBreak =
  DOM.div
    { className: "mitt-konto--disappearing-break"
    , children: [ break ]
    }

loadingIndicator :: Spinner.Loading -> JSX
loadingIndicator Spinner.Loading =
  DOM.div
    { className: "mitt-konto--loading flex items-center"
    , children:
        [ DOM.div
            { className: "clearfix mx-auto"
            , children: [ DOM.div { className: "mitt-konto--loading-image", children: [] } ]
            }
        ]
    }
