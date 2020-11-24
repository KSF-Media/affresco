module MittKonto.IconAction where

import Prelude

import Effect (Effect)
import React.Basic (JSX, element, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Router as Router

type Self = React.Self Props {}

data OnClick
  = Href String
  | Router String
  | Action (Effect Unit)

type Props =
  { iconClassName :: String
  , description :: String
  , onClick :: OnClick
  }

iconAction :: Props -> JSX
iconAction = make component
  { initialState: {}
  , render
  }

component :: React.Component Props
component = React.createComponent "IconAction"

render :: Self -> JSX
render self =
  case self.props.onClick of
    Href url -> accountActionAnchor url
    Router path -> routerLink path
    Action onClick -> accountActionContainer

  where
    accountActionContainer =
      DOM.div
        { className: "icon-action--container"
        , onClick: handler_ case self.props.onClick of
          Action onClickAction -> onClickAction
          _ -> pure unit
        , children:
            [ DOM.div { className: self.props.iconClassName }
            , DOM.div_ [ DOM.text self.props.description ]
            ]
        }

    accountActionAnchor href =
      DOM.a
        { href
        , children: [ accountActionContainer ]
        , target: "_blank"
        }

    routerLink pathname =
      element
        Router.link
          { to: { pathname, state: {} }
          , children: [ accountActionContainer ]
          , className: mempty
          }
