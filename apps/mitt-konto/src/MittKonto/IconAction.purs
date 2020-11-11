module MittKonto.IconAction where

import Prelude

import Effect (Effect)
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Self = React.Self Props {}

data OnClick
  = Href String Boolean -- TODO: Boolean can be removed when "/fakturor" uses React Router
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
    Href url openInNewTab -> accountActionAnchor url openInNewTab
    Action onClick -> accountActionContainer

  where
    accountActionContainer =
      DOM.div
        { className: "icon-action--container"
        , onClick: handler_ case self.props.onClick of
          Action onClickAction -> onClickAction
          Href _ _ -> pure unit
        , children:
            [ DOM.div { className: self.props.iconClassName }
            , DOM.div_ [ DOM.text self.props.description ]
            ]
        }

    accountActionAnchor href openNewTab =
      DOM.a
        { href
        , children: [ accountActionContainer ]
        , target: if openNewTab then "_blank" else ""
        }
