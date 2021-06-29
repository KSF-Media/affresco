module KSF.AsyncWrapper where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM
import Data.Maybe (Maybe)

type Props a =
  { wrapperState :: Progress a
  , readyView    :: JSX
  , editingView  :: a -> JSX
  , loadingView  :: (JSX -> JSX)
  , successView  :: Maybe String -> JSX
  , errorView    :: String -> JSX
  }

data Progress a
  = Ready
  | Editing a
  | Loading a
  | Success (Maybe String)
  | Error String

derive instance functorProgress :: Functor Progress


asyncWrapper :: forall a. Props a -> JSX
asyncWrapper props = case props.wrapperState of
  Ready       -> props.readyView
  Editing a   -> props.editingView a
  Loading _   -> props.loadingView loadingSpinner
  Success msg -> props.successView msg
  Error msg   -> props.errorView msg

loadingSpinner :: JSX
loadingSpinner = DOM.div { className: "tiny-spinner right" }
