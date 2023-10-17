module Tracking where

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Function.Uncurried (Fn4, runFn4)
import KSF.User (OrderNumber)
import Prelude (Unit)

foreign import transaction_ :: Fn4 OrderNumber String String String (Effect Unit)
foreign import orderStarted_ :: Effect Unit

transaction
  :: OrderNumber
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Effect Unit
transaction orderNumber (Just productId) (Just productPrice) maybeCampaignNo =
  runFn4 transaction_ orderNumber productId productPrice (fold maybeCampaignNo)
transaction orderNumber _ _ _ = runFn4 transaction_ orderNumber "" "" ""

orderStarted :: Effect Unit
orderStarted = orderStarted_