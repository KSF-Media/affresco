module KSF.Button.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events as Event
import Record (merge)

type Props =
  { description :: String
  , destination :: Maybe String
  , onClick :: Effect Unit
  }

render :: Props -> JSX
render props =
  case props.destination of
    Just href -> DOM.a $ merge attrs { href, target: "_blank" }
    _ -> DOM.a attrs
  where
    attrs =
      { className: "button--blank-button button--noselect"
      , children:
          [ DOM.text props.description ]
      , onClick: Event.handler_ props.onClick
      }
