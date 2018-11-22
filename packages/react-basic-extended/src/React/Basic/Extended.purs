module React.Basic.Extended
  ( module React.Basic.Compat
  , ComponentSpec
  , RenderArgs
  , ReceivePropsArgs
  , SetState
  , SetStateThen
  , contramapComponentProps
  , Style
  , requireStyle
  ) where

import React.Basic.Compat

import Data.Function (($))
import Data.Unit (Unit)
import Effect (Effect)
import React.Basic (ReactComponentInstance)

-- | In v4.0.0 this will be replaced with 'Update'.
type SetState state = (state -> state) -> Effect Unit

-- | In v4.0.0 this will be replaced with 'Update' (and 'UpdateAndSideEffects').
type SetStateThen state =
     (state -> state)
  -> (state -> Effect Unit)
  -> Effect Unit

-- | In v4.0.0 this will be replaced by 'Self'
type RenderArgs props state =
  { props :: props
  , state :: state
  , setState :: SetState state
  , setStateThen :: SetStateThen state
  , instance_ :: ReactComponentInstance
  }

-- | In v4.0.0 this will be replaced by 'Self'
type ReceivePropsArgs props state =
  { isFirstMount :: Boolean
  , props :: props
  , state :: state
  , setState :: SetState state
  , setStateThen :: SetStateThen state
  , instance_ :: ReactComponentInstance
  }

-- | Will be provided in v4.0.0
type ComponentSpec props state =
  { displayName :: String
  , initialState :: state
  , receiveProps :: ReceivePropsArgs props state -> Effect Unit
  , render :: RenderArgs props state -> JSX
  }

-- | Convert the props of component spec.
contramapComponentProps
  :: forall state propsA propsB
   . (propsB -> propsA)
  -> ComponentSpec propsA state
  -> ComponentSpec propsB state
contramapComponentProps mapProps spec = spec
  { receiveProps = \self -> spec.receiveProps $ self
      { props = mapProps self.props }
  , render = \self -> spec.render $ self
      { props = mapProps self.props }
  }

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
