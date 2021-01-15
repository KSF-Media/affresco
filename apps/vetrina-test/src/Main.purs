module VetrinaTest.Main where

import KSF.Vetrina (Props, JSProps)
import KSF.Vetrina as Vetrina
import React.Basic (JSX)
import React.Basic.Classic as React

type State = { }
type Self = React.Self Props State

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent Vetrina.fromJSProps component { initialState: {}, render }

component :: React.Component Props
component = React.createComponent "VetrinaTest"

render :: Self -> JSX
render self =
  Vetrina.vetrina self.props
