module KSF.Button.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events as Event
import Record (merge)

type Props =
  { description :: String
  , destination :: Maybe String
  , onClick :: Effect Unit
  }

type State = { }

component :: React.Component Props
component = React.createComponent "Button"

button :: Props -> JSX
button = make component
  { initialState: {}
  , render
  }

render :: React.Self Props State -> JSX
render { props } =
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
