module KSF.Alert where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM

newtype Level = Level String

danger :: Level
danger = Level "danger"

warning :: Level
warning = Level "warning"

info :: Level
info = Level "info"

success :: Level
success = Level "success"

type Alert =
  { level   :: Level
  , title   :: String
  , message :: String
  }

type Props = Alert

render :: Props -> JSX
render props =
  DOM.div
    { className: "alert" <> " " <> alertLevelClass props.level
    , children:
        [ DOM.div
            { className: "alert__title"
            , children: [ DOM.text props.title ]
            }
        , DOM.div
            { className: "alert__message"
            , children: [ DOM.text props.message ]
            }
        ]
    }
  where
    alertLevelClass (Level level) = "alert--" <> level
