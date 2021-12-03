module HtmlRenderer where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type Self =
  { props :: Props
  }

type Props =
  { content     :: String
  , sanitize    :: Boolean
  }

htmlRendererComponent :: Component Props
htmlRendererComponent = do
  component "HtmlRenderer" \props -> React.do
    pure $ render { props }

render :: Self -> JSX
render { props } = DOM.div_ [ DOM.text "Hello World!" ]
