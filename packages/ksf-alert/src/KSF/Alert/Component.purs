module KSF.Alert.Component where

import Prelude

import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as JSX
import React.Basic.Extended as React.Extended

foreign import alertStyles :: React.Extended.Style

newtype Level = Level String

type Self = React.Self Props {}

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
component = React.createComponent "Alert"

alert :: Props -> JSX
alert = make component
  { initialState: {}
  , render: React.Extended.requireStyle alertStyles <<< render
  }


render :: Self -> JSX
render { props: alert_ } =
  JSX.div
    { className: "alert" <> " " <> alertLevelClass alert_.level
    , children:
        [ JSX.div
            { className: "alert__title"
            , children: [ JSX.text alert_.title ]
            }
        , JSX.div
            { className: "alert__message"
            , children: [ JSX.text alert_.message ]
            }
        ]
    }
  where
    alertLevelClass (Level level) = "alert--" <> level
