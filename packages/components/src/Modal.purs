module KSF.Modal where

import React.Basic (JSX, make)
import React.Basic as React
import React.Basic.DOM as DOM

data Visibility = Visible | Hidden

type Self = React.Self Props {}

type Props =
  { content :: JSX
  }

component :: React.Component Props
component = React.createComponent "Modal"

modal :: Props -> JSX
modal = make component
  { initialState: {}
  , render
  }

render :: Self -> JSX
render { props } =
  DOM.div
    { className: "modal--container"
    , children: [ props.content ]
    }