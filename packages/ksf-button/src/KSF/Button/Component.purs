module KSF.Button.Component where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception as Exception
import KSF.Button.View as View
import React.Basic (JSX)
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
component = React.component { displayName: "Button", render, initialState: {}, receiveProps }
  where
    receiveProps { instance_, props, isFirstMount } = when isFirstMount do
      node <- DOM.findDOMNode instance_
      either Exception.throwException props.onLoad node

render :: forall r. { props :: Props | r } -> JSX
render { props } =
  View.button
    { description: props.description
    , destination: props.destination
    , onClick: props.onClick
    }
