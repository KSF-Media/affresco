module KSF.Footer.Component where

import KSF.Footer.View as View
import React.Basic (JSX, createComponent, make)
import React.Basic as React

type Self = React.Self Props {}
type Props = {}

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = createComponent "Footer"

reactComponent :: React.ReactComponent {}
reactComponent =
  React.toReactComponent (\_ -> {}) component { render, initialState: {} }

footer :: Props -> JSX
footer = make component
  { initialState: {}
  , render
  }

render :: Self -> JSX
render _ = View.footer
