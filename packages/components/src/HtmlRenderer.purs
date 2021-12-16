module KSF.HtmlRenderer where

import Prelude

import KSF.HtmlRenderer.Models as Models
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

foreign import renderHtmlInput :: String -> JSX

type Self =
  { props :: Props
  }

type Props =
  { content :: String
  }

htmlRendererComponent :: Component Props
htmlRendererComponent = do
  component "HtmlRenderer" \props -> React.do
    pure $ render { props }

render :: Self -> JSX
render { props } = DOM.div_ [ renderHtmlInput props.content ]
