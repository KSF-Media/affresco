module Main where

import Prelude

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
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import KSF.Api (Token(..), UserAuth)
import Lettera as Lettera
import Mosaico.Article as Article
import MosaicoServer as MosaicoServer
import Node.Encoding (Encoding(..))
import Lettera.Models (fromFullArticle, articleToJson, isPreviewArticle)
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
import React.Basic.DOM.Server as DOM

-- NOTE: We need to require dotenv in JS
foreign import requireDotenv :: Unit
foreign import appendMosaico :: EffectFn2 String String String
foreign import writeArticle :: EffectFn3 Json Boolean String String

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
  htmlTemplate <- liftEffect $ FS.readTextFile UTF8 indexHtmlFileLocation
  articleComponent <- liftEffect Article.articleComponent
  mosaico <- liftEffect MosaicoServer.app
  case article of
    Right a -> do
      let articleJSX =
            articleComponent
              { brand: "hbl"
              , affArticle: pure a
              , articleStub: Nothing
              , onLogin: pure unit
              , user: Nothing
              , article: Just a
              }
          mosaicoString = DOM.renderToString $ mosaico { mainContent: articleJSX }

      html <- liftEffect do
        runEffectFn2 appendMosaico htmlTemplate mosaicoString
          >>= runEffectFn3 writeArticle (articleToJson $ fromFullArticle a) (isPreviewArticle a)

      pure $ TextHtml html
    Left err -> do
      Console.warn $ "Could not get article: " <> err
      pure $ TextHtml "Could not get article"

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: { path } } = Handlers.directory "dist/client" path

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
