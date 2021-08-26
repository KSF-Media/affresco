module Main where

import Data.Argonaut.Encode.Class
import Lettera.Models
import Prelude
import Simple.JSON

import Cheerio as Cheerio
import Cheerio.Static (loadRoot) as Cheerio
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn2, runEffectFn2)
import KSF.Api (Token(..), UserAuth)
import Lettera as Lettera
import Lettera.Models (fromFullArticle)
import Mosaico as Mosaico
import Mosaico.Article as Article
import Mosaico.Header as Header
import MosaicoServer as MosaicoServer
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
import React.Basic.DOM as DOM
import React.Basic.DOM.Server as DOM
import Unsafe.Coerce (unsafeCoerce)

-- NOTE: We need to require dotenv in JS
foreign import requireDotenv :: Unit
foreign import appendMosaico :: EffectFn2 String String String
foreign import addArticle :: EffectFn2 String Json String

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
  let uuidString = UUID.toString $ fromMaybe UUID.emptyUUID $ UUID.parseUUID uuid
  article <- Lettera.getArticle (fromMaybe UUID.emptyUUID $ UUID.parseUUID uuid) r.guards.credentials
  html <- liftEffect $ FS.readTextFile UTF8 indexHtmlFileLocation
  articleComponent <- liftEffect Article.articleComponent
  mosaico <- liftEffect MosaicoServer.app
  case article of
    Right a' -> do
      let wast = articleComponent { brand: "hbl", affArticle: pure a', articleStub: Nothing, onLogin: pure unit, user: Nothing, article: Just a' } -- html
          mosaicoString = DOM.renderToString $ mosaico { mainContent: wast }
      html' <- liftEffect $ runEffectFn2 appendMosaico html mosaicoString
      html2 <- liftEffect $ runEffectFn2 addArticle html' $ articleToJson $ fromFullArticle a'
      --let aa = writeJSON $ fromFullArticle a'
      pure $ TextHtml html2
    Left err -> do
      Console.warn $ "Could not get article: " <> err
      pure $ TextHtml "aa" ---html

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
