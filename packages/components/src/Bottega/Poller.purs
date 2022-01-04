module Bottega.Poller
   ( Poller
   , new
   , startOrder
   , kill
   )
where

import Prelude

import Bottega (BottegaError)
import Bottega.Models (OrderNumber, OrderState(..))
import Control.Monad.Rec.Class (untilJust)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.AVar (AVar, empty)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import KSF.User as User

type State = Either BottegaError OrderState

type Poller =
  { poller :: AVar (Aff.Fiber Unit)
  , lock :: AVar Unit
  }

new :: Effect Poller
new = do
  poller <- empty
  lock <- empty
  pure
    { poller
    , lock
    }

startOrder :: Poller -> (State -> Effect Unit) -> OrderNumber -> Aff Unit
startOrder poller setState order = do
  AVar.put unit poller.lock
  liftEffect $ setState $ Right OrderCreated
  poller_ <- Aff.forkAff do
    kill_ poller
    let wait = Aff.delay (Aff.Milliseconds 1000.0)
    wait
    untilJust do
      let continue = wait *> pure Nothing
      status <- map (_.status.state <<< unwrap) <$> User.getOrder order
      liftEffect $ setState status
      case status of
        Right OrderCreated -> continue
        Right OrderStarted -> continue
        _ -> pure $ Just unit
  AVar.put poller_ poller.poller
  AVar.take poller.lock

kill_ :: Poller -> Aff Unit
kill_ { poller } =
  AVar.tryTake poller >>= foldMap (Aff.killFiber (error "Cancel"))

kill :: Poller -> Aff Unit
kill poller = do
  AVar.put unit poller.lock
  kill_ poller
  AVar.take poller.lock
