-- | This module is a small PureScript wrapper around the react-date-picker package.
--   The original react-date-picker docs can be found in https://www.npmjs.com/package/react-date-picker
module DatePicker.Component where

import Prelude

import Data.Function.Uncurried (Fn0, runFn0)
import Data.JSDate (JSDate)
import Data.Nullable (Nullable)
import Effect.Uncurried (EffectFn1)
import React.Basic (JSX, ReactComponent)
import React.Basic as React

foreign import datePicker_ :: Fn0 (ReactComponent Props)

type Props =
  { onChange  :: EffectFn1 (Nullable JSDate) Unit
  , className :: String
  , value     :: Nullable JSDate
  , format    :: String
  , required  :: Boolean
  , minDate   :: Nullable JSDate
  , maxDate   :: Nullable JSDate
  , disabled  :: Boolean
  , locale    :: String
  }

datePicker :: Props -> JSX
datePicker = React.element $ runFn0 datePicker_
