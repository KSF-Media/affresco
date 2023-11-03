module Persona.Identification where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Argonaut as JSON
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex as Regex
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.AVar as AVar
import Effect.Aff.AVar as Aff.AVar
import Foreign as Foreign
import Foreign.Object (lookup)
import KSF.Api (Token(..))
import KSF.Auth as Auth
import KSF.User (User)
import Persona (personaURL)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onClose, onError, onMessage)
import Web.Socket.Event.MessageEvent (data_, fromEvent)
import Web.Socket.WebSocket as WS

-- This part doesn't use the generated JavaScript client.  The monitor
-- endpoint is missing from it in the first place (since it's not in
-- swagger) and the rest are used just by directing the user's browser
-- to them.
type Monitor =
  -- Blocks until monitor WebSocket is up
  { monitor :: Aff String
  -- Blocks until identification process is done.  The user facing
  -- part only cares that it succeeds.
  , result :: Aff (Either String Unit)
  }

getMonitor :: User -> Effect Monitor
getMonitor user = do
  { authToken: Token token } <- Auth.requireToken
  let url = fromMaybe "" do
        regex <- hush $ Regex.regex "^http" mempty
        pure $ Regex.replace regex "ws" $
          personaURL <> "/identification/login/monitor/makeToken/" <> UUID.toString user.uuid <>
          -- The WebSockets API doesn't allow custom headers so it
          -- needs to be done via query.
          "?authorization=" <> token
  ws <- WS.create url []
  monitor <- AVar.empty
  result <- AVar.empty
  let et = WS.toEventTarget ws
      getField a = JSON.toString <=< lookup a
      useMessage obj | Just "MonitorSuccess" <- getField "tag" obj
                     = do
        tryPut (Right unit) result
      useMessage obj | Just "MonitorSendToken" <- getField "tag" obj
                     , Just monitorToken <- getField "token" obj
                     = do
        tryPut monitorToken monitor
      -- TODO
      useMessage obj | Just val <- getField "error" obj = tryPut (Left val) result
      useMessage _ = pure unit
  messageListener <- eventListener $ \event -> case fromEvent event of
    Nothing -> pure unit
    Just mEvent -> do
      eitherJson <- runExceptT $ JSON.jsonParser <$> (Foreign.readString $ data_ mEvent)
      case eitherJson of
        Left err -> do
          tryPut (Left $ "foreign: " <> show err) result
        Right (Left err) -> do
          tryPut (Left $ "json: " <> show err) result
        Right (Right json) ->
          JSON.caseJsonObject (pure unit) useMessage json
  -- Relies on tryPut's do nothing-semantics if messageListener already fired
  closeListener <- eventListener $ const $ AVar.tryPut (Left "connection closed") result
  errorListener <- eventListener $ const $ do
    _ <- AVar.tryPut "" monitor
    _ <- AVar.tryPut (Left "WS error") result
    pure unit

  addEventListener onMessage messageListener false et
  addEventListener onClose closeListener false et
  addEventListener onError errorListener false et

  pure $ { monitor: Aff.AVar.read monitor
         , result: Aff.AVar.read result
         }
  where
    tryPut :: forall a. a -> AVar.AVar a -> Effect Unit
    tryPut a ref = AVar.tryPut a ref *> pure unit

-- Make sure that the monitor is up before using this or it'll fail.
-- No parameters from here but the monitor has set up a HTTP only
-- cookie for it.
loginURL :: String -> String
loginURL token = personaURL <> "/identification/login?monitor=" <> token
