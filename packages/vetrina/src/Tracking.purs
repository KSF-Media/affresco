module Tracking where

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import KSF.User (OrderNumber)
import Prelude (Unit, pure, unit)

foreign import transaction_ :: EffectFn4 OrderNumber String String String Unit

transaction
  :: OrderNumber
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Effect Unit
transaction orderNumber (Just productId) (Just productPrice) maybeCampaignNo =
  runEffectFn4 transaction_ orderNumber productId productPrice (fold maybeCampaignNo)
transaction orderNumber _ _ _ = pure unit
