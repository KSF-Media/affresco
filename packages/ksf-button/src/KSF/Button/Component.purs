module KSF.Button.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events as Event
import Record (merge)
import Web.DOM as Web.DOM

type Props =
  { description :: String
  , destination :: Maybe String
  , onClick :: Effect Unit
  , onLoad :: Web.DOM.Node -> Effect Unit
  }

component :: React.Component Props
component = React.createComponent "Button"

button :: Props -> JSX
button = make component
  { initialState: {}
  , render
  , didMount
  }

didMount :: React.Self Props {} -> Effect Unit
didMount { instance_, props } = do
  mountedNode <- DOM.findDOMNode instance_
  case mountedNode of
    Left err -> do
      Console.error $ "Button component: " <> Error.message err
    Right node -> do
      props.onLoad node

render :: React.Self Props {} -> JSX
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
