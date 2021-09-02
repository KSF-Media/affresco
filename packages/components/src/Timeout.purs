module KSF.Timeout where

import Prelude

import Data.Maybe (maybe)
import Data.Time.Duration (class Duration, convertDuration)
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Aff as Aff
import Effect.Aff.AVar as Aff.AVar
import Effect.AVar (AVar)
import Effect.AVar as AVar
import Effect.Exception (error)

type Timer =
  { timerFiber :: AVar (Fiber Unit)
  , lock :: AVar Unit
  }

newTimer :: Effect Timer
newTimer = do
  timerFiber <- AVar.empty
  lock <- AVar.empty
  pure { timerFiber, lock }

stopTimer :: Timer -> Aff Unit
stopTimer { lock, timerFiber } = Aff.invincible do
  Aff.AVar.put unit lock
  maybe (pure unit) (Aff.killFiber $ error "stop") =<< Aff.AVar.tryTake timerFiber
  Aff.AVar.take lock

startTimer :: forall a. Duration a => a -> Timer -> Aff Unit -> Aff Unit
startTimer duration { lock, timerFiber } action = Aff.invincible do
  Aff.AVar.put unit lock
  maybe (pure unit) (Aff.killFiber $ error "restart") =<< Aff.AVar.tryTake timerFiber
  flip Aff.AVar.put timerFiber =<< Aff.forkAff do
    Aff.delay $ convertDuration duration
    action
    Aff.AVar.put unit lock
    void $ Aff.AVar.take timerFiber
    Aff.AVar.put unit lock
  Aff.AVar.take lock
