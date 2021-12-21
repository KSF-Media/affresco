module KSF.HtmlRenderer where

import Prelude

import Data.Function.Uncurried
import Data.Maybe (Maybe(..))
import KSF.HtmlRenderer.Models as Models
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

foreign import renderHtmlInputImpl          :: Fn1 String JSX
foreign import renderHtmlInputWithHooksImpl :: Fn2 String (Array Models.JSGenericHook) JSX

renderHtmlInput :: String -> JSX
renderHtmlInput htmlInput = runFn1 renderHtmlInputImpl htmlInput

renderHtmlInputWithHooks :: String -> Array Models.JSGenericHook -> JSX
renderHtmlInputWithHooks htmlInput hooks = runFn2 renderHtmlInputWithHooksImpl htmlInput hooks

type Self =
  { props :: Props
  }

type Props =
  { content :: String
  , hooks   :: Maybe (Array Models.HookRep)
  }

htmlRendererComponent :: Component Props
htmlRendererComponent = do
  component "HtmlRenderer" \props -> React.do
    pure $ render { props }

render :: Self -> JSX
render { props } =
  let content = case props.hooks of
                  Nothing    -> renderHtmlInput props.content
                  Just hooks -> renderHtmlInputWithHooks props.content $ hooks <#> \(Models.HookRep h) -> h (Models.toGenericHook >>> Models.toJSGenericHook)
   in  DOM.div_ [ content ]