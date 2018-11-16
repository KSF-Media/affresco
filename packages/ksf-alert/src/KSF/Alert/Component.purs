module KSF.Alert.Component where

import Prelude
import React.Basic.DOM as JSX
import React.Basic.Extended (JSX, Style)
import React.Basic.Extended as React

foreign import alertStyles :: Style

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

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.stateless
  { displayName: "Alert"
  , render: React.requireStyle alertStyles <<< render
  }

render :: Props -> JSX
render alert =
  JSX.div
    { className: "alert" <> " " <> alertLevelClass alert.level
    , children:
        [ JSX.div
            { className: "alert__title"
            , children: [ JSX.text alert.title ]
            }
        , JSX.div
            { className: "alert__message"
            , children: [ JSX.text alert.message ]
            }
        ]
    }
  where
    alertLevelClass (Level level) = "alert--" <> level
