module MittKonto.Main.UserView.IconAction where

import Prelude

import Effect (Effect)
import Foreign (unsafeToForeign)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import Routing.PushState (PushStateInterface)

data OnClick
  = Href String
  | Router String
  | Action (Effect Unit)

type Props =
  { iconClassName :: String
  , description :: String
  , onClick :: OnClick
  , router :: PushStateInterface
  }

render :: Props -> JSX
render props =
  case props.onClick of
    Href url -> accountActionAnchor url
    Router path -> routerLink path
    Action _onClick -> accountActionContainer

  where
    accountActionContainer =
      DOM.div
        { className: "icon-action--container"
        , onClick: handler_ case props.onClick of
          Action onClickAction -> onClickAction
          _ -> pure unit
        , children:
            [ DOM.div { className: props.iconClassName }
            , DOM.div_ [ DOM.text props.description ]
            ]
        }

    accountActionAnchor href =
      DOM.a
        { href
        , children: [ accountActionContainer ]
        , target: "_blank"
        }

    routerLink pathname =
      DOM.div
        { onClick: handler preventDefault $ const $ props.router.pushState (unsafeToForeign {}) pathname
        , children: [ accountActionContainer ]
        }
