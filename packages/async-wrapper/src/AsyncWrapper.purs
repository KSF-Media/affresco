module AsyncWrapper where

import React.Basic (JSX)
import React.Basic.DOM as DOM

type Props =
  { wrapperState :: Progress
  , readyView    :: JSX
  , editingView  :: JSX
  , errorView    :: JSX
  }

data Progress
  = Ready
  | Editing
  | Loading
  | Error

type Views =
  { readyView :: JSX
  , editingView :: JSX
  , errorView :: JSX
  }

asyncWrapper :: Props -> JSX
asyncWrapper props = case props.wrapperState of
  Ready   -> props.readyView
  Editing -> props.editingView
  Loading -> loadingSpinner
  Error   -> props.errorView

loadingSpinner :: JSX
loadingSpinner = DOM.div { className: "tiny-spinner right" }
