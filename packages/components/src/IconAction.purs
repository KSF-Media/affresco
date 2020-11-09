module KSF.IconAction where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)


type Self = React.Self Props State

data ActionOnClick
  = Href String
  | Custom (Effect Unit)

type Props =
  { iconClassName :: String
  , description :: String
  , href :: String
  , onClick :: ActionOnClick
  }

type State =
  { }

iconAction :: Props -> JSX
iconAction = make component
  { initialState:
      {   }
  , render

  }

component :: React.Component Props
component = React.createComponent "IconAction"

render :: Self -> JSX
render self =
  DOM.div
    { className: "icon-action--container"
    , onClick: handler_ case self.props.onClick of
      Custom o -> o
      Href _ -> pure unit
    , children:
        [ DOM.div { className: self.props.iconClassName }
        , DOM.div_ $ Array.singleton
            case self.props.onClick of
              Href url -> accountEditAnchor url self.props.description
              Custom _ -> DOM.text self.props.description
        ]
    }


accountEditAnchor :: String -> String -> JSX
accountEditAnchor href description =
  DOM.a
    { href
    , className: ""
    , children: [ DOM.text description ]
    , target: "_blank"
    }
