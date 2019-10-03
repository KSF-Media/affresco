module KSF.AsyncWrapper where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM

type Props a =
  { wrapperState :: Progress a
  , readyView    :: JSX
  , editingView  :: a -> JSX
  , loadingView  :: (JSX -> JSX)
  , successView  :: JSX
  , errorView    :: String -> JSX
  }

data Progress a
  = Ready
  | Editing a
  | Loading a
  | Success
  | Error String

derive instance functorProgress :: Functor Progress


asyncWrapper :: forall a. Props a -> JSX
asyncWrapper props = case props.wrapperState of
  Ready     -> props.readyView
  Editing a -> props.editingView a
  Loading a -> props.loadingView loadingSpinner
  Success   -> props.successView
  Error msg -> props.errorView msg

loadingSpinner :: JSX
loadingSpinner = DOM.div { className: "tiny-spinner right" }
