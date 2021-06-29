module Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe, fromMaybe)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import KSF.Api (Token(..), UserAuth)
import Lettera as Lettera
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
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
instance encodeResponsePlainHtml :: EncodeResponse TextHtml where
  encodeResponse (Response res) = do
    let (TextHtml b) = res.body
    pure $
      Response
        { status: res.status
        , headers: Headers.setIfNotDefined "content-type" ContentType.html res.headers
        , body: StringBody b
        }

indexHtmlFileLocation :: String
indexHtmlFileLocation = "./dist/client/index.html"

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
    , guards :: { credentials :: Maybe UserAuth }
    }
spec = Spec

main :: Effect Unit
main = do
  let handlers = { getArticle, assets, frontpage }
      guards = { credentials: getCredentials }
  Aff.launchAff_ $ Payload.startGuarded (Payload.defaultOpts { port = 8080 }) spec { handlers, guards }

getArticle :: { params :: { uuid :: String }, guards :: { credentials :: Maybe UserAuth } } -> Aff TextHtml
getArticle r@{ params: { uuid } } = do
  article <- Lettera.getArticle (fromMaybe UUID.emptyUUID $ UUID.parseUUID uuid) r.guards.credentials
  html <- liftEffect $ FS.readTextFile UTF8 indexHtmlFileLocation
  case article of
    -- TODO: Add rendering for the article we get here
    -- Now we are just returning the pre-built stuff Parcel gives us
    Right _ -> pure $ TextHtml html
    Left err -> do
      Console.warn $ "Could not get article: " <> err
      pure $ TextHtml html

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: {path} } = Handlers.directory "dist/client" path

frontpage :: { guards :: { credentials :: Maybe UserAuth } } -> Aff TextHtml
frontpage _ = do
  html <- liftEffect $ FS.readTextFile UTF8 indexHtmlFileLocation
  pure $ TextHtml html

getCredentials :: HTTP.Request -> Aff (Maybe UserAuth)
getCredentials req = do
  headers <- Guards.headers req
  let tokens = do
        authToken <- Token <$> Headers.lookup "Authorization" headers
        userId <- UUID.parseUUID =<< Headers.lookup "Auth-User" headers
        pure { authToken, userId }
  pure tokens
