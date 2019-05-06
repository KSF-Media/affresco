module AsyncWrapper where

import React.Basic (JSX)
import React.Basic.DOM as DOM

type Props =
  { wrapperState :: Progress
  , readyView    :: JSX
  , editingView  :: JSX
  , successView  :: JSX
  , errorView    :: String -> JSX
  }

data Progress
  = Ready
  | Editing
  | Loading
  | Success
  | Error String

asyncWrapper :: Props -> JSX
asyncWrapper props = case props.wrapperState of
  Ready     -> props.readyView
  Editing   -> props.editingView
  Loading   -> loadingSpinner
  Success   -> props.successView
  Error msg -> props.errorView msg

loadingSpinner :: JSX
loadingSpinner = DOM.div { className: "tiny-spinner right" }
