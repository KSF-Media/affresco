module AsyncWrapper where

import React.Basic (JSX)
import React.Basic.DOM as DOM

type Props =
  { wrapperState :: Progress
  , readyView    :: JSX
  , editingView  :: JSX
  , successView  :: JSX
  , errorView    :: JSX
  }

data Progress
  = Ready
  | Editing
  | Loading
  | Success
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
  Success -> props.successView
  Error   -> props.errorView

loadingSpinner :: JSX
loadingSpinner = DOM.div { className: "tiny-spinner right" }
