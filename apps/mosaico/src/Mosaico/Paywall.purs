module Mosaico.Paywall where

import Prelude

import Lettera.Models (Article)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

type Props = { article :: Article }

paywall :: Component {}
paywall = do
  component "Paywall" \_ -> React.do
    state /\ setState <- useState {}
    pure render

render :: JSX
render =
  DOM.div
   { className: "mosaico--paywall"
   , children: []
   }
