module Ospedale.Login where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.AVar as AVar
import Effect.Aff.AVar as Aff.AVar
import Effect.Class (liftEffect)
import Foreign as Foreign
import Foreign.Object (lookup)
import Lettera.Fallback (fallbackMonitorIntrospect, fallbackMonitorUrl)
import Web.Event.EventTarget (addEventListener, addEventListenerWithOptions, eventListener)
import Web.Socket.Event.EventTypes (onClose, onMessage, onOpen)
import Web.Socket.Event.MessageEvent (data_, fromEvent)
import Web.Socket.WebSocket as WS
import Web.Storage.Storage as Storage
import Debug
--import Data.Generic.Rep as Rep
--import Data.Generic.Rep.Show (genericShow)
--import Data.Storage (class Storable, StorageError(..))

type Session =
  { session :: String
  , email :: String
  }

type Monitor =
  { monitor :: Aff String
  , result :: Aff (Either String Session)
  }

getMonitor :: Storage.Storage -> Effect Monitor
getMonitor storage = do
  traceM {fallbackMonitorUrl}
  ws <- WS.create fallbackMonitorUrl []
  monitor <- AVar.empty
  result <- AVar.empty
  let et = WS.toEventTarget ws
      once = { capture: false, once: true, passive: false }
      getField a = JSON.toString <=< lookup a
      useMessage obj | Just "MonitorToken" <- getField "tag" obj
                     , Just key <- getField "key" obj = tryPut key monitor
      useMessage obj | Just "MonitorSuccess" <- getField "tag" obj
                     , Just contents <- JSON.toObject =<< lookup "contents" obj
                     , Just email <- getField "email" contents
                     , Just session <- getField "token" contents
                     = do
        Storage.setItem "ospedale-session" session storage
        tryPut (Right {session, email}) result
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

removeSession :: Storage.Storage -> Effect Unit
removeSession = Storage.removeItem "ospedale-session"

recoverSession :: Storage.Storage -> Effect (Maybe (Aff (Maybe Session)))
recoverSession storage = do
  stored <- Storage.getItem "ospedale-session" storage
  pure $ flip foldMap stored $ \session -> Just $ do
    introspect <- fallbackMonitorIntrospect session
    case introspect of
      Right email -> pure $ Just {session, email}
      _ -> do
        liftEffect $ removeSession storage
        pure Nothing
