module Tracking where

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import KSF.User (OrderNumber)
import Prelude (Unit, pure, unit)

foreign import data Tracker :: Type
foreign import transaction_ :: Fn3 OrderNumber String String (Effect Unit)

transaction :: OrderNumber -> Maybe String -> Maybe String -> Effect Unit
transaction ord (Just t) (Just p)  = runFn3 transaction_ ord t p
transaction ord _ _ = pure unit
