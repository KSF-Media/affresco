module Ospedale.Login where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.AVar as AVar
import Effect.Aff.AVar as Aff.AVar
import Foreign as Foreign
import Foreign.Object (lookup)
import Ospedale.TokenResponse (TokenResponse)
import Ospedale.TokenResponse as TokenResponse
import Web.Event.EventTarget (addEventListener, addEventListenerWithOptions, eventListener)
import Web.Socket.Event.EventTypes (onClose, onMessage, onOpen)
import Web.Socket.Event.MessageEvent (data_, fromEvent)
import Web.Socket.WebSocket as WS
import Debug

type Result =
  { token :: TokenResponse.AccessToken
  , name :: String
  }

type Monitor =
  { monitor :: Aff String
  , result :: Aff (Either String Result)
  }

getMonitor :: Effect Monitor
getMonitor = do
  ws <- WS.create "ws://localhost:8081/v4/fallback/login/monitor" []
  monitor <- AVar.empty
  result <- AVar.empty
  let et = WS.toEventTarget ws
      once = { capture: false, once: true, passive: false }
      getField a = JSON.toString <=< lookup a
      useMessage obj | Just "MonitorToken" <- getField "tag" obj
                     , Just key <- getField "key" obj = tryPut key monitor
      useMessage obj | Just "MonitorSuccess" <- getField "tag" obj
                     , Just contents <- JSON.toObject =<< lookup "contents" obj
                     , Just refreshURL <- getField "refreshURL" contents
                     , Just name <- getField "name" contents
                     , Just refreshToken <- TokenResponse.fromJSON =<< lookup "tokenResponse" contents = do
        -- TODO the refresh token (and URL?) could be stored
        token <- TokenResponse.maintainAccess refreshURL refreshToken
        tryPut (Right {token, name}) result
      -- TODO
      useMessage obj | Just val <- getField "error" obj = tryPut (Left val) result
      useMessage _ = pure unit
  openListener <- eventListener $ const $ WS.sendString ws "\"key\""
  messageListener <- eventListener $ \event -> case fromEvent event of
    Nothing -> pure unit
    Just mEvent -> do
      eitherJson <- runExceptT $ JSON.jsonParser <$> (Foreign.readString $ data_ mEvent)
      traceM {eitherJson}
      case eitherJson of
        Left err -> do
          tryPut (Left $ "foreign: " <> show err) result
          traceM {error: "foreign", err}
        Right (Left err) -> do
          tryPut (Left $ "json: " <> show err) result
          traceM {error: "json", err}
        Right (Right json) ->
          JSON.caseJsonObject (pure unit) (\x -> trace {msg:x} $ const $ trace {useMessage:x} $ const $ useMessage x) json
  -- Relies on tryPut's do nothing-semantics if messageListener already fired
  closeListener <- eventListener $ const $ AVar.tryPut (Left "connection closed") result

  addEventListener onMessage messageListener false et
  addEventListener onClose closeListener false et
  addEventListenerWithOptions onOpen openListener once et

  pure $ { monitor: Aff.AVar.read monitor
         , result: Aff.AVar.read result
         }
  where
    tryPut :: forall a. a -> AVar.AVar a -> Effect Unit
    tryPut a ref = AVar.tryPut a ref *> pure unit
