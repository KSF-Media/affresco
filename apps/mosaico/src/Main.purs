module Main where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (cons, null)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, EffectFn5, runEffectFn2, runEffectFn5)
import KSF.Api (Token(..), UserAuth)
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Lettera.Models (ArticleStub, DraftParams, FullArticle, articleStubToJson, articleToJson, fromFullArticle, isPreviewArticle, isDraftArticle, notFoundArticle)
import Mosaico.Article as Article
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
import Payload.Server.Response as Response
import Payload.Spec (type (:), GET, Guards, Spec(Spec), Nil)
import React.Basic.DOM.Server as DOM

foreign import appendMosaicoImpl :: EffectFn2 String String String
appendMosaico :: String -> String -> Effect String
appendMosaico htmlTemplate content = runEffectFn2 appendMosaicoImpl htmlTemplate content

foreign import writeArticleImpl :: EffectFn5 Json Boolean Json Boolean String String
writeArticle :: Json -> Boolean -> Json -> Boolean -> String -> Effect String
writeArticle = runEffectFn5 writeArticleImpl

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

type Env =
  { htmlTemplate :: String }

indexHtmlFileLocation :: String
indexHtmlFileLocation = "./dist/client/index.html"

spec ::
  Spec
    { routes ::
         { getDraftArticle ::
              GET "/artikel/draft/<aptomaId>/?dp-time=<time>&publicationId=<publication>&user=<user>&hash=<hash>"
                { response :: ResponseBody
                , params :: { aptomaId :: String }
                , query :: DraftParams
                }
         , getArticle ::
              GET "/artikel/<uuid>"
                { response :: ResponseBody
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
          , notFound ::
              GET "/<..path>"
                { response :: ResponseBody
                , params :: { path :: List String}
                }
         }
    , guards :: { credentials :: Maybe UserAuth }
    }
spec = Spec

main :: Effect Unit
main = do
  htmlTemplate <- liftEffect $ FS.readTextFile UTF8 indexHtmlFileLocation
  let env = { htmlTemplate }
      handlers =
        { getDraftArticle: getDraftArticle env
        , getArticle: getArticle env
        , assets
        , frontpage
        , notFound: notFound Nothing
        }
      guards = { credentials: getCredentials }
  Aff.launchAff_ $ Payload.startGuarded (Payload.defaultOpts { port = 8080 }) spec { handlers, guards }

getDraftArticle
  :: Env
  -> { params :: { aptomaId :: String }, query :: DraftParams }
  -> Aff (Response ResponseBody)
getDraftArticle env { params: {aptomaId}, query } = do
  article <- Lettera.getDraftArticle aptomaId query
  renderArticle env Nothing article mempty

getArticle
  :: Env
  -> { params :: { uuid :: String }, guards :: { credentials :: Maybe UserAuth } }
  -> Aff (Response ResponseBody)
getArticle env r@{ params: { uuid } } = do
  article <- Lettera.getArticle (fromMaybe UUID.emptyUUID $ UUID.parseUUID uuid) r.guards.credentials
  mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
  renderArticle env (Just uuid) article mostReadArticles

renderArticle
  :: Env
  -> Maybe String
  -> Either String FullArticle
  -> Array ArticleStub
  -> Aff (Response ResponseBody)
renderArticle { htmlTemplate } uuid article mostReadArticles = do
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
              , uuid
              }
          mosaicoString = DOM.renderToString $ mosaico { mainContent: articleJSX, mostReadArticles }

      html <- liftEffect do

        appendMosaico htmlTemplate mosaicoString
          >>= writeArticle
                (articleToJson $ fromFullArticle a)
                (isPreviewArticle a)
                (encodeJson $ map articleStubToJson mostReadArticles)
                (isDraftArticle a)

      pure $ Response.ok $ StringBody html
    Left _ ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
      in notFound maybeMostRead { params: {path: foldMap (List.fromFoldable <<< (_ `cons` ["artikel"])) uuid} }

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: { path } } = Handlers.directory "dist/client" path

frontpage :: { guards :: { credentials :: Maybe UserAuth } } -> Aff TextHtml
frontpage _ = do
  html <- liftEffect $ FS.readTextFile UTF8 indexHtmlFileLocation
  pure $ TextHtml html

notFound :: Maybe (Array ArticleStub) -> { params :: { path :: List String } } -> Aff (Response ResponseBody)
notFound mostReadList _ = do
  htmlTemplate <- liftEffect $ FS.readTextFile UTF8 indexHtmlFileLocation
  mostReadArticles <- maybe (Lettera.getMostRead 0 10 "" HBL true) pure mostReadList
  articleComponent <- liftEffect Article.articleComponent
  let articleJSX =
        articleComponent
          { brand: "hbl"
          , affArticle: pure notFoundArticle
          , articleStub: Nothing
          , onLogin: pure unit
          , user: Nothing
          , article: Just notFoundArticle
          , uuid: Nothing
          }

  mosaico <- liftEffect MosaicoServer.app
  let mosaicoString = DOM.renderToString $ mosaico { mainContent: articleJSX, mostReadArticles }
  html <- liftEffect $
    appendMosaico htmlTemplate mosaicoString

  pure $ Response.notFound $ StringBody $ html

getCredentials :: HTTP.Request -> Aff (Maybe UserAuth)
getCredentials req = do
  headers <- Guards.headers req
  let tokens = do
        authToken <- Token <$> Headers.lookup "Authorization" headers
        userId <- UUID.parseUUID =<< Headers.lookup "Auth-User" headers
        pure { authToken, userId }
  pure tokens
