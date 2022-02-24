module KSF.Error where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Maybe (Maybe, isJust, isNothing)
import Control.MonadPlus (guard)
import Data.Traversable (traverse)
import Effect.Exception (Error, message)
import Foreign (Foreign, unsafeToForeign, readNullOrUndefined)
import Foreign.Index as Foreign
import Simple.JSON (class ReadForeign)
import Simple.JSON as JSON

isRedirect :: Error -> Boolean
isRedirect err = isJust $ do
  status <- err # errorField "status"
  guard $ status >= 300 && status < 400

badRequestError :: Error -> Boolean
badRequestError err = isJust $ do
  status <- err # errorField "status"
  guard $ status == 400

resourceConflictError :: Error -> Boolean
resourceConflictError err = isJust $ do
  status <- err # errorField "status"
  guard $ status == 409

loginExpiredError :: Error -> Boolean
loginExpiredError err = isJust do
  status <- err # errorField "status"
  guard $ status == 403
  guard $ message err == "login_token_expired"

-- | Matches internal server error produced by superagent.
--   Checks that it has `status` field that's 5XX.
internalServerError :: Error -> Maybe { status :: Int }
internalServerError err = do
  status <- err # errorField "status"
  guard $ status >= 500 && status <= 599
  pure { status }

serviceUnavailableError :: Error -> Boolean
serviceUnavailableError err = isJust $ do
  status <- err # errorField "status"
  guard $ status == 503

-- | Matches network error produced by superagent.
--   Checks that it has `method` and `url` fields, but no `status`.
networkError :: Error -> Maybe { method :: String, url :: String }
networkError err = do
  method <- errorField "method" err
  url <- errorField "url" err
  guard $ isNothing (errorField "status" err :: Maybe Foreign)
  pure { method, url }

-- | Check if an error has some field and it's not null or undefined.
errorField :: forall a. ReadForeign a => String -> Error -> Maybe a
errorField field =
  join <<< hush
    <<< (traverse JSON.read =<< _)
    <<< runExcept
    <<< do readNullOrUndefined <=< Foreign.readProp field
    <<< unsafeToForeign
