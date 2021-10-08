module KSF.Api.Error where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect.Exception (Error)
import Foreign (unsafeToForeign)
import Foreign.Index as Foreign
import Simple.JSON (class ReadForeign)
import Simple.JSON as JSON

type ServerError extraFields =
  { http_status :: String
  , http_code :: Int
  | extraFields
  }

errorData
  :: forall fields
   . ReadForeign (ServerError fields)
  => Error
  -> Maybe (ServerError fields)
errorData =
  hush
    <<< (JSON.read =<< _)
    <<< runExcept
          <<< Foreign.readProp "data"
          <<< unsafeToForeign
