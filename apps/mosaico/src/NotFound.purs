module NotFound where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React

type Self =
  { 
    props :: Props
  }

type Props =
  { path :: Array String }

notFoundComponent :: Component Props
notFoundComponent = do
  component "NotFound" \props -> React.do
    pure $ render { props }

render :: Self -> JSX
render { props } =
  DOM.div
    { className: "not-found"
    , children:
      [ DOM.h1_
        [ DOM.text "Hoppsan! Sidan eller artikeln hittades inte" ]
      ] 
    }