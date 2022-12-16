module Prenumerera.Gift where

import Prelude

import Bottega.Models as Bottega
import Data.UUID (UUID)

type Gift =
  { owner :: UUID
  , code  :: String
  }
