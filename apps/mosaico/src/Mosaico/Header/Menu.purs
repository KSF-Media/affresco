module Mosaico.Header.Menu where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useState, (/\))
import React.Basic.Hooks as React

type Self =
  { props :: Props
  }

type Props =
  { visible :: Boolean
  }

menuComponent :: Component Props
menuComponent = do
  component "Menu" \props -> React.do
    pure $ render { props }

render :: Self -> JSX
render self = DOM.div
  { className: block <> "__menu"
  , children: [ DOM.text "menu" ]
  }
  where
    block = "mosaico-header"