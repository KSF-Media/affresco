module Tracking where

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import KSF.User (OrderNumber)
import Prelude (Unit, pure, unit)

foreign import transaction_ :: Fn3 OrderNumber String String (Effect Unit)

transaction :: OrderNumber -> Maybe String -> Maybe String -> Effect Unit
transaction orderNumber (Just productId) (Just productPrice)  = runFn3 transaction_ orderNumber productId productPrice
transaction orderNumber _ _ = pure unit
