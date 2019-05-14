module React.Basic.Extended
  ( module React.Basic.Compat
  , SetState
  , SetStateThen
  , Style
  , requireStyle
  ) where

import React.Basic.Compat

import Data.Unit (Unit)
import Effect (Effect)

-- | In v4.0.0 this will be replaced with 'Update'.
type SetState state = (state -> state) -> Effect Unit

-- | In v4.0.0 this will be replaced with 'Update' (and 'UpdateAndSideEffects').
type SetStateThen state =
     (state -> state)
  -> (state -> Effect Unit)
  -> Effect Unit

-- | A dummy type to give to CSS files that we `import` from foreign modules
--   so that parcel adds them to global stylesheet.
foreign import data Style :: Type

-- | A dummy function that allows to "annotate" a JSX value with a style that it requires.
--   Such reference will prevent the `purs bundle` from eliminating a seemingly dead reference
--   to the style require, so that it can be successfully resolved by the parcel.
--
--   Here's a pattern that you can use for adding styles:
--
--       // MyComponent.js
--       exports.myStyle = require("myStyle.less");
--
--       -- MyComponent.purs
--       foreign import myStyle :: React.Style
--
--       myView :: JSX
--       myView = React.requireStyle myStyle $ DOM.div_ [ DOM.text "my view" ]
requireStyle :: Style -> JSX -> JSX
requireStyle _ jsx = jsx
