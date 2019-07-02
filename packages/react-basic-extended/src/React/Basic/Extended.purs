module React.Basic.Extended
  ( module React.Basic.Compat
  , SetState
  , SetStateThen
  ) where

import           React.Basic.Compat

import           Data.Unit          (Unit)
import           Effect             (Effect)

-- | In v4.0.0 this will be replaced with 'Update'.
type SetState state = (state -> state) -> Effect Unit

-- | In v4.0.0 this will be replaced with 'Update' (and 'UpdateAndSideEffects').
type SetStateThen state =
     (state -> state)
  -> (state -> Effect Unit)
  -> Effect Unit
