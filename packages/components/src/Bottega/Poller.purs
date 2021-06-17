module Bottega.Poller where

import Prelude

import Bottega (BottegaError)
import Bottega.Models (OrderNumber, OrderState(..))
import Control.Monad.Rec.Class (untilJust)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
import KSF.User as User

type Poller =
  { orderState :: Either BottegaError OrderState
  , poller :: Maybe (Aff.Fiber Unit)
  , onComplete :: Aff Unit
  }

new :: Poller
new =
  { orderState: Right OrderUnknownState
  , poller: Nothing
  , onComplete: pure unit
  }

start :: Poller -> ((Poller -> Poller) -> Effect Unit) -> OrderNumber -> Effect Unit
start poller setPoller order = do
  poller_ <- Aff.launchAff do
    foldMap (Aff.killFiber (error "Cancel")) poller.poller
    let wait = Aff.delay (Aff.Milliseconds 1000.0)
    wait
    untilJust do
      let continue = wait *> pure Nothing
      status <- map _.status.state <$> User.getOrder order
      liftEffect $ setPoller _ { orderState = status }
      case status of
        Right OrderCreated -> continue
        Right OrderStarted -> continue
        _ -> pure $ Just unit
    poller.onComplete
  setPoller _ { poller = Just poller_ }
