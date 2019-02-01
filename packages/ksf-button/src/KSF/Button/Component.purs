module KSF.Button.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import KSF.Button.View as View
import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM
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

didMount :: React.Self Props {} Void -> Effect Unit
didMount { instance_, props } = do
  mountedNode <- DOM.findDOMNode instance_
  case mountedNode of
    Left err -> do
      Console.error $ "Button component: " <> Error.message err
    Right node -> do
      props.onLoad node

render :: React.Self Props {} Void -> JSX
render { props } =
  View.button
    { description: props.description
    , destination: props.destination
    , onClick: props.onClick
    }
