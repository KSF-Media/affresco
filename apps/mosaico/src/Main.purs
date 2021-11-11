module Main where

import Prelude

import Data.Argonaut.Core as JSON
import Data.Argonaut.Encode (encodeJson)
import Data.Array (cons, null)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, foldM)
import Data.HashMap as HashMap
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object as Object
import KSF.Api (Token(..), UserAuth)
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Lettera.Models (ArticleStub, DraftParams, FullArticle, encodeStringifyArticle, encodeStringifyArticleStubs, fromFullArticle, isDraftArticle, isPreviewArticle, notFoundArticle, tagToURIComponent, uriComponentToTag)
import Mosaico.Article as Article
import Mosaico.Error as Error
import Mosaico.Frontpage as Frontpage
import Mosaico.StaticPage (StaticPageResponse(..), fetchStaticPage)
import MosaicoServer (MainContent(..))
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
import Payload.Server.Status as Status
import Payload.Spec (type (:), GET, Guards, Spec(Spec), Nil)
import React.Basic (JSX)
import React.Basic.DOM (div) as DOM
import React.Basic.DOM.Server (renderToString) as DOM

foreign import appendMosaicoImpl :: EffectFn2 String String String
appendMosaico :: String -> String -> Effect String
appendMosaico content htmlTemplate = runEffectFn2 appendMosaicoImpl content htmlTemplate

foreign import appendHeadImpl :: EffectFn2 String String String
appendHead :: String -> String -> Effect String
appendHead = runEffectFn2 appendHeadImpl

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
  { htmlTemplate :: String
  , staticPages :: HashMap.HashMap String String
  }

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
              GET "/artikel/<uuidOrSlug>"
                { response :: ResponseBody
                , params :: { uuidOrSlug :: String }
                , guards :: Guards ("credentials" : Nil)
                }
         , assets ::
              GET "/assets/<..path>"
                { params :: { path :: List String }
                , response :: File
                }
         , tagList ::
              GET "/tagg/<tag>"
                { response :: TextHtml
                , params :: { tag :: String }
                , guards :: Guards ("credentials" : Nil)
                }
         , frontpage ::
              GET "/"
                { response :: TextHtml
                , guards :: Guards ("credentials" : Nil)
                }
         , staticPage ::
              GET "/sida/<pageName>"
                { response :: ResponseBody
                , params :: { pageName :: String }
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
  staticPages  <- do
    staticPageNames <- FS.readdir "./static/"
    let makeMap acc staticPageFileName = do
          pageContent <- FS.readTextFile UTF8 $ "./static/" <> staticPageFileName
          let staticPageName = replace (Pattern ".html") (Replacement "") staticPageFileName
          pure $ HashMap.insert staticPageName pageContent acc
    foldM makeMap HashMap.empty staticPageNames
  htmlTemplate <- liftEffect $ FS.readTextFile UTF8 indexHtmlFileLocation
  let env = { htmlTemplate, staticPages }
      handlers =
        { getDraftArticle: getDraftArticle env
        , getArticle: getArticle env
        , assets
        , tagList: tagList env
        , frontpage: frontpage env
        , staticPage: staticPage env
        , notFound: notFound env Nothing
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
  -> { params :: { uuidOrSlug :: String }, guards :: { credentials :: Maybe UserAuth } }
  -> Aff (Response ResponseBody)
getArticle env r@{ params: { uuidOrSlug } }
  | Just uuid <- UUID.parseUUID uuidOrSlug = do
      article <- Lettera.getArticle uuid r.guards.credentials
      mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
      renderArticle env (Just uuidOrSlug) article mostReadArticles
  | otherwise = do
    article <- Lettera.getArticleWithSlug uuidOrSlug r.guards.credentials
    case article of
      Right a -> do
        pure $ Response
          { status: Status.found
          , body: EmptyBody
          , headers: Headers.fromFoldable [ Tuple "Location" $ "/artikel/" <> (_.uuid $ fromFullArticle a)]
          }
      Left _ -> do
        mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
        let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
        notFound env maybeMostRead { params: {path: List.fromFoldable ["artikel", uuidOrSlug]} }

renderArticle
  :: Env
  -> Maybe String
  -> Either String FullArticle
  -> Array ArticleStub
  -> Aff (Response ResponseBody)
renderArticle env@{ htmlTemplate } uuid article mostReadArticles = do
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
          mosaicoString = DOM.renderToString $ mosaico { mainContent: ArticleContent articleJSX, mostReadArticles }

      html <- liftEffect do
        let windowVars  =
              "<script>\
                \window.article=" <> (encodeStringifyArticle $ fromFullArticle a) <> ";\
                \window.isPreview=" <> (show $ isPreviewArticle a) <> ";\
                \window.mostReadArticles=" <> encodeStringifyArticleStubs mostReadArticles <> ";\
                \window.isDraft=" <> (show $ isDraftArticle a) <> ";\
              \</script>"
        appendMosaico mosaicoString htmlTemplate >>= appendHead windowVars

      pure $ Response.ok $ StringBody html
    Left _ ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
      in notFound env maybeMostRead { params: {path: foldMap (List.fromFoldable <<< (_ `cons` ["artikel"])) uuid} }

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: { path } } = Handlers.directory "dist/client" path

frontpage :: Env -> { guards :: { credentials :: Maybe UserAuth } } -> Aff TextHtml
frontpage { htmlTemplate } _ = do
  articles <- Lettera.getFrontpage HBL
  mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
  mosaico <- liftEffect MosaicoServer.app
  frontpageComponent <- liftEffect Frontpage.frontpageComponent
  let mosaicoString =
        DOM.renderToString
        $ mosaico
          { mainContent:
              FrontpageContent
              $ frontpageComponent
                  { frontpageArticles: articles
                  , onArticleClick: const $ pure unit
                  , onTagClick: const $ pure unit
                  }
          , mostReadArticles
          }
  html <- liftEffect do
            let windowVars =
                  "<script>\
                     \window.frontpageArticles=" <> encodeStringifyArticleStubs articles <> ";\
                     \window.mostReadArticles="  <> encodeStringifyArticleStubs mostReadArticles <> ";\
                  \</script>"
            appendMosaico mosaicoString htmlTemplate >>= appendHead windowVars
  pure $ TextHtml html

tagList :: Env -> { params :: { tag :: String }, guards :: { credentials :: Maybe UserAuth } } -> Aff TextHtml
tagList { htmlTemplate } { params: { tag } } = do
  let tag' = uriComponentToTag tag
  articles <- Lettera.getByTag 0 20 tag' HBL
  mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
  mosaico <- liftEffect MosaicoServer.app
  frontpageComponent <- liftEffect Frontpage.frontpageComponent
  let mosaicoString =
        DOM.renderToString
        $ mosaico
          { mainContent:
              TagListContent
              $ frontpageComponent
                  { frontpageArticles: articles
                  , onArticleClick: const $ pure unit
                  , onTagClick: const $ pure unit
                  }
          , mostReadArticles
          }
  html <- liftEffect do
            let windowVars =
                  "<script>\
                     \window.tagListArticles=" <> encodeStringifyArticleStubs articles <> ";\
                     \window.tagListArticlesName=\"" <> tagToURIComponent tag' <> "\";\
                     \window.mostReadArticles="  <> encodeStringifyArticleStubs mostReadArticles <> ";\
                  \</script>"
            appendMosaico mosaicoString htmlTemplate >>= appendHead windowVars
  pure $ TextHtml html

staticPage :: Env -> { params :: { pageName :: String }} -> Aff (Response ResponseBody)
staticPage env { params: { pageName } } = do
  let staticPageResponse =
        case HashMap.lookup pageName env.staticPages of
          Just staticPageContent -> StaticPageResponse { pageName, pageContent: staticPageContent }
          Nothing -> StaticPageNotFound
  mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
  case staticPageResponse of
    StaticPageNotFound ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
      in notFound env maybeMostRead { params: {path: List.fromFoldable ["sida", pageName]} }
    p -> do
      mosaico <- liftEffect MosaicoServer.app
      let staticPageContent :: Either JSX String
          staticPageContent =
            case p of
              StaticPageResponse page -> Right page.pageContent
              StaticPageOtherError -> Left Error.somethingWentWrong
              StaticPageNotFound -> Left mempty
      let staticPageJsx =
            case staticPageContent of
              Right pageContent ->
                DOM.div { className: "mosaico--static-page"
                        , dangerouslySetInnerHTML: { __html: pageContent }
                        }
              Left jsx -> jsx
      let mosaicoString =
            DOM.renderToString
            $ mosaico
              { mainContent: StaticPageContent staticPageJsx
              , mostReadArticles
              }
      html <- liftEffect do
        let staticPageString = JSON.stringify $ JSON.fromString $ either DOM.renderToString identity staticPageContent
            staticPageObj = Object.singleton "pageName" pageName
                            # Object.insert "pageContent" staticPageString
                            # encodeJson
                            # JSON.stringify
        appendMosaico mosaicoString env.htmlTemplate
          >>= appendHead ("<script>window.staticPageContent=" <> staticPageObj <> ";</script>")

      pure $ Response.ok $ StringBody html

notFound :: Env -> Maybe (Array ArticleStub) -> { params :: { path :: List String } } -> Aff (Response ResponseBody)
notFound { htmlTemplate } _ _ = do
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
  let mosaicoString = DOM.renderToString $ mosaico { mainContent: ArticleContent articleJSX, mostReadArticles: [] }
  html <- liftEffect $ do
    let windowVars  =
              "<script>\
                \window.article=" <> (encodeStringifyArticle $ fromFullArticle notFoundArticle) <> ";\
              \</script>"
    appendMosaico mosaicoString htmlTemplate >>= appendHead windowVars
  pure $ Response.notFound $ StringBody $ html

getCredentials :: HTTP.Request -> Aff (Maybe UserAuth)
getCredentials req = do
  headers <- Guards.headers req
  let tokens = do
        authToken <- Token <$> Headers.lookup "Authorization" headers
        userId <- UUID.parseUUID =<< Headers.lookup "Auth-User" headers
        pure { authToken, userId }
  pure tokens
