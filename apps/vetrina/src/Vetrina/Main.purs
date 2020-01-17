module Vetrina.Main where

import Prelude

import React.Basic (JSX)
import React.Basic.Compat as React
import React.Basic.DOM as DOM

type Props = {}
type State = {}

app :: React.Component Props
app = React.component
  { displayName: "Vetrina"
  , initialState: {}
  , receiveProps
  , render
  }
  where
    receiveProps _ = do
      pure unit

    render { state, setState } =
      DOM.h1_ [ DOM.text "Vetrina!" ]
