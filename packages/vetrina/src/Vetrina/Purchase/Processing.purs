module Vetrina.Purchase.Processing where

import React.Basic.Classic (JSX, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM

type Self = React.Self Props State
type Props = {}
type State = {}

component :: React.Component Props
component = React.createComponent "PurchaseCompleted"

processing :: Props -> JSX
processing = make component
  { initialState: {}
  , render
  }

render :: Self -> JSX
render self = DOM.div_ [DOM.text "Vänligen vänta. Vi behandlar din beställning."]
