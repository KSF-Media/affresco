module KSF.Sentry where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toNullable)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
import KSF.User as User
import KSF.User.Cusno (Cusno(..))


foreign import initSentry_       :: EffectFn2 String Number Sentry
foreign import captureMessage_   :: EffectFn4 Sentry String String String Unit
foreign import captureException_ :: EffectFn3 Sentry String Error Unit
foreign import setTag_           :: EffectFn3 Sentry String String Unit
foreign import setUser_          :: EffectFn2 Sentry (Nullable Int) Unit
foreign import data Sentry       :: Type

data LogLevel
  = Fatal
  | Error
  | Warning
  | Info
  | Debug
derive instance genericLogLevel :: Generic LogLevel _
instance showLogLevel :: Show LogLevel where
  show = toLower <<< genericShow

newtype SessionId = SessionId UUID.UUID

type Logger =
  { log     :: String -> LogLevel -> Effect Unit
  , setUser :: Maybe User.User -> Effect Unit
  , error   :: Error -> Effect Unit
  , setTag  :: String -> String -> Effect Unit
  }

emptyLogger :: Logger
emptyLogger =
  { log: \_msg _level -> Console.warn "Tried to log to Sentry, but it's not initialized"
  , setUser: const $ pure unit
  , error: const $ pure unit
  , setTag: \_ _ -> pure unit
  }

-- Sentry logger using default sapmpling rate
mkLogger :: String -> Maybe User.User -> String -> Effect Logger
mkLogger sentryDsn = mkLogger' sentryDsn Nothing

-- Sentry logger with a custom sampling rate
mkLoggerWithSR :: String -> Number -> Maybe User.User -> String -> Effect Logger
mkLoggerWithSR sentryDsn samplingRate = mkLogger' sentryDsn (Just samplingRate)

mkLogger' :: String -> Maybe Number -> Maybe User.User -> String -> Effect Logger
mkLogger' sentryDsn maybeSR maybeUser appNameTag = do
  sentry <- runEffectFn2 initSentry_ sentryDsn $ fromMaybe 0.8 maybeSR
  sessionId <- UUID.genUUID
  -- Set sessionId to Sentry
  -- This is to batch requests together if no User if ever set.
  runEffectFn3 setTag_ sentry "sessionId" $ UUID.toString sessionId
  -- Set cusno to Sentry
  setUser sentry maybeUser
  pure
    { log: log sentry appNameTag
    , setUser: setUser sentry
    , error: captureException sentry appNameTag
    , setTag: setTag sentry
    }

setUser :: Sentry -> Maybe User.User -> Effect Unit
setUser sentry user = runEffectFn2 setUser_ sentry $ toNullable (((\(Cusno c) -> c) <<< _.cusno) <$> user)

setTag :: Sentry -> String -> String -> Effect Unit
setTag sentry tagName tagValue = runEffectFn3 setTag_ sentry tagName tagValue

log :: Sentry -> String -> String -> LogLevel -> Effect Unit
log sentry appNameTag msg level = runEffectFn4 captureMessage_ sentry appNameTag msg $ show level

captureException :: Sentry -> String -> Error -> Effect Unit
captureException sentry appNameTag = runEffectFn3 captureException_ sentry appNameTag
