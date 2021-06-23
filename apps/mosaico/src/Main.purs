module Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Lettera as Lettera
import Node.FS.Sync as FS
import Node.Encoding (Encoding (..))
import Node.HTTP as HTTP
import Payload.ContentType as ContentType
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure, Response(..), ResponseBody(..))
import Payload.Server as Payload
import Payload.Server.Guards as Guards
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as Handlers
import Payload.Server.Response (class EncodeResponse)
import Payload.Spec (type (:), GET, Guards, Spec(Spec), Nil)

-- NOTE: We need to require dotenv in JS
foreign import requireDotenv :: Unit

newtype TextHtml = TextHtml String

type Credentials = { userId :: UUID, token :: String }

instance encodeResponsePlainHtml :: EncodeResponse TextHtml where
  encodeResponse (Response res) = do
    let (TextHtml b) = res.body
    pure $
      Response
        { status: res.status
        , headers: Headers.setIfNotDefined "content-type" ContentType.html res.headers
        , body: StringBody b
        }

spec ::
  Spec
    { routes ::
         { getArticle ::
              GET "/artikel/<uuid>"
                { response :: TextHtml
                , params :: { uuid :: String }
                , guards :: Guards ("credentials" : Nil)
                }
         , assets ::
              GET "/assets/<..path>"
                { params :: { path :: List String }
                , response :: File
                }
         , frontpage ::
              GET "/"
                { response :: TextHtml
                , guards :: Guards ("credentials" : Nil)
                }
         }
    , guards :: { credentials :: Maybe Credentials }
    }
spec = Spec

main :: Effect Unit
main = do
  let handlers = { getArticle, assets, frontpage }
      guards = { credentials: getCredentials }
  Aff.launchAff_ $ Payload.startGuarded_ spec { handlers, guards }

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: {path} } = Handlers.directory "dist/client" path

frontpage :: { guards :: { credentials :: Maybe Credentials } } -> Aff TextHtml
frontpage _ = do
  html <- liftEffect$ FS.readTextFile UTF8 "./dist/client/index.html"
  pure $ TextHtml html

getArticle :: { params :: { uuid :: String }, guards :: { credentials :: Maybe Credentials } } -> Aff TextHtml
getArticle r@{ params: { uuid } } = do
  case r.guards.credentials of
    -- TODO: Pass credentials to Lettera
    Just _  -> Console.log "YES CREDS!"
    Nothing -> Console.log "NO CREDS!"
  article <- Lettera.getArticle (fromMaybe UUID.emptyUUID $ UUID.parseUUID uuid) Nothing
  case article of
    Right _ -> do
      html <- liftEffect$ FS.readTextFile UTF8 "./dist/client/index.html"
      pure $ TextHtml html
    Left err -> do
      Console.warn $ "Could not get article: " <> err
      pure $ TextHtml "Något gick fel :~("

getCredentials :: HTTP.Request -> Aff (Maybe Credentials)
getCredentials req = do
  headers <- Guards.headers req
  let tokens = do
        token  <- Headers.lookup "Authorization" headers
        userId <- UUID.parseUUID =<< Headers.lookup "Auth-User" headers
        pure { token, userId }
  pure tokens
