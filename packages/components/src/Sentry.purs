module KSF.Sentry where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import KSF.User as User

newtype SentryIssueId = SentryIssueId String

foreign import initSentry_       :: EffectFn1 String Sentry
foreign import captureMessage_   :: EffectFn3 Sentry String String SentryIssueId
foreign import captureException_ :: EffectFn2 Sentry String SentryIssueId
foreign import setExtra_         :: EffectFn3 Sentry String UUID.UUID Unit
foreign import setUser_          :: EffectFn2 Sentry String Unit
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
  { log :: String -> LogLevel -> Effect SentryIssueId }



mkLogger :: String -> Maybe User.User -> Effect Logger
mkLogger sentryDsn maybeUser = do
  sentry <- runEffectFn1 initSentry_ sentryDsn
  sessionId <- UUID.genUUID
  -- Set sessionId to Sentry
  -- This is to batch requests together if no User if ever set.
  runEffectFn3 setExtra_ sentry "sessionId" sessionId
  -- Set cusno to Sentry
  setUser sentry maybeUser
  pure $ { log: log sentry }

setUser :: Sentry -> Maybe User.User -> Effect Unit
setUser sentry (Just user) = runEffectFn2 setUser_ sentry user.cusno
setUser _ Nothing = pure unit

log :: Sentry -> String -> LogLevel -> Effect SentryIssueId
log sentry msg level = runEffectFn3 captureMessage_ sentry msg $ show level

captureException :: Sentry -> String -> Effect SentryIssueId
captureException sentry = runEffectFn2 captureException_ sentry
