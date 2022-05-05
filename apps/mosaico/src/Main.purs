module Main where

import Prelude

import Control.Parallel.Class (parallel, sequential)
import Data.Argonaut.Core as JSON
import Data.Argonaut.Encode (encodeJson)
import Data.Array (cons, find, foldl, fromFoldable, null)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), hush)
import Data.Foldable (fold, foldM, foldMap, elem)
import Data.HashMap as HashMap
import Data.List (List, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String (trim)
import Data.String.Regex (Regex)
import Data.String.Regex (match, regex) as Regex
import Data.String.Regex.Flags (ignoreCase) as Regex
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object (lookup)
import JSURI as URI
import KSF.Paper as Paper
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category(..), CategoryLabel(..), DraftParams, FullArticle, ArticleType(..), encodeStringifyArticle, encodeStringifyArticleStubs, frontpageCategoryLabel, notFoundArticle, uriComponentToTag)
import Mosaico.Article as Article
import Mosaico.Article.Advertorial.Basic as Advertorial.Basic
import Mosaico.Article.Advertorial.Standard as Advertorial.Standard
import Mosaico.Article.Image as Image
import Mosaico.Cache (Stamped(..))
import Mosaico.Cache as Cache
import Mosaico.Epaper as Epaper
import Mosaico.Error (notFoundWithAside)
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), mkArticleFeed)
import Mosaico.Frontpage (Frontpage(..), render) as Frontpage
import Mosaico.Frontpage.Models (Hook(..)) as Frontpage
import Mosaico.Header.Menu as Menu
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Profile as Profile
import Mosaico.Search as Search
import MosaicoServer (MainContent, MainContentType(..))
import MosaicoServer as MosaicoServer
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.HTTP as HTTP
import Payload.ContentType as ContentType
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure(..), Response(..), ResponseBody(..))
import Payload.Server as Payload
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as Handlers
import Payload.Server.Response as Response
import Payload.Server.Status as Status
import Payload.Spec (type (:), GET, Guards, Spec(Spec), Nil)
import React.Basic (fragment) as DOM
import React.Basic.DOM (div, meta, script, text, title) as DOM
import React.Basic.DOM.Server (renderToStaticMarkup, renderToString) as DOM
import React.Basic.Events (handler_)
import Routing.PushState (PushStateInterface)
import Simple.JSON (write)

foreign import data Template :: Type
foreign import data TemplateMaster :: Type

foreign import parseTemplate :: String -> TemplateMaster
foreign import cloneTemplate :: TemplateMaster -> Template
foreign import renderTemplateHtml :: Template -> String

foreign import appendMosaicoImpl :: EffectFn2 String Template Template
appendMosaico :: String -> Template -> Effect Template
appendMosaico content htmlTemplate = runEffectFn2 appendMosaicoImpl content htmlTemplate

foreign import appendHeadImpl :: EffectFn2 String Template Template
appendHead :: String -> Template -> Effect Template
appendHead content htmlTemplate =
  runEffectFn2 appendHeadImpl
  ("<link rel='icon' href='https://cdn.ksfmedia.fi/mosaico/favicon/" <> Paper.cssName mosaicoPaper <> "/favicon.svg'/>" <> content)
  htmlTemplate

foreign import appendVarsImpl :: EffectFn2 String Template Template
appendVars :: String -> Template -> Effect Template
appendVars = runEffectFn2 appendVarsImpl

foreign import serverPort :: Int
foreign import globalDisableAds :: Boolean

type Env =
  { htmlTemplate :: TemplateMaster
  , categoryStructure :: Array Category
  , categoryRegex :: Regex
  , staticPages :: HashMap.HashMap String String
  , cache :: Cache.Cache
  }

indexHtmlFileLocation :: String
indexHtmlFileLocation = "./dist/index.html"

spec ::
  Spec
    { routes ::
         { getHealthz ::
              GET "/healthz"
                { response :: String
                , guards :: Guards ("clientip" : Nil)
                }
         , frontpageUpdated ::
              GET "/api/reset/<category>"
                { params :: { category :: String }
                , response :: String
                }
         , getDraftArticle ::
              GET "/artikel/draft/<aptomaId>/?dp-time=<time>&publicationId=<publication>&user=<user>&hash=<hash>"
                { response :: ResponseBody
                , params :: { aptomaId :: String }
                , query :: DraftParams
                }
         , getArticle ::
              GET "/artikel/<uuidOrSlug>"
                { response :: ResponseBody
                , params :: { uuidOrSlug :: String }
                , guards :: Guards ("clientip" :Nil)
                }
         , assets ::
              GET "/assets/<..path>"
                { params :: { path :: List String }
                , response :: File
                }
         , tagList ::
              GET "/tagg/<tag>"
                { response :: ResponseBody
                , params :: { tag :: String }
                }
         , frontpage ::
              GET "/"
                { response :: ResponseBody }
         , menu ::
              GET "/meny"
                { response :: ResponseBody }
         , profilePage ::
              GET "/konto"
                { response :: ResponseBody
                }
         , staticPage ::
              GET "/sida/<pageName>"
                { response :: ResponseBody
                , params :: { pageName :: String }
                }
         , epaperPage ::
              GET "/epaper"
                { response :: ResponseBody
                }
         , debugList ::
              GET "/debug/<uuid>"
                { response :: ResponseBody
                , params :: { uuid :: String }
                }
          , categoryPage ::
              GET "/<categoryName>"
                { response :: ResponseBody
                , params :: { categoryName :: String }
                , guards :: Guards ("category" : Nil)
                }
          , searchPage ::
              GET "/sök?q=<search>"
                { response :: ResponseBody
                , query :: { search :: Maybe String }
                }
          , notFoundPage ::
              GET "/<..path>"
                { response :: ResponseBody
                , params :: { path :: List String}
                }
         }
    , guards ::
         { category :: Category
         , clientip :: Maybe String
         }
    }
spec = Spec

main :: Effect Unit
main = do
  staticPages  <- do
      staticPageNames <- FS.readdir "./static/"
      let makeMap acc staticPageFileName = do
            pageContent <- FS.readTextFile UTF8 $ "./static/" <> staticPageFileName
            pure $ HashMap.insert staticPageFileName pageContent acc
      foldM makeMap HashMap.empty staticPageNames
  htmlTemplate <- parseTemplate <$> FS.readTextFile UTF8 indexHtmlFileLocation
  Aff.launchAff_ do
    categoryStructure <- Lettera.getCategoryStructure mosaicoPaper
    cache <- liftEffect $ Cache.initCache mosaicoPaper categoryStructure
    -- This is used for matching a category label from a route, such as "/nyheter" or "/norden-och-världen"
    categoryRegex <- case Regex.regex "^\\/([\\w|ä|ö|å|-]+)\\b" Regex.ignoreCase of
      Right r -> pure r
      Left _  -> liftEffect $ throw "I have a very safe regex to parse, yet somehow I didn't know how to parse it. Fix it please. Exploding now, goodbye."
    let env = { htmlTemplate, categoryStructure, categoryRegex, staticPages, cache }
        handlers =
          { getHealthz
          , frontpageUpdated: frontpageUpdated env
          , getDraftArticle: getDraftArticle env
          , getArticle: getArticle env
          , assets
          , frontpage: frontpage env
          , tagList: tagList env
          , staticPage: staticPage env
          , epaperPage: epaperPage env
          , debugList: debugList env
          , categoryPage: categoryPage env
          , searchPage: searchPage env
          , notFoundPage: notFoundPage env
          , profilePage: profilePage env
          , menu: menu env
          }
        guards =
          { category: parseCategory env
          , clientip: getClientIP
          }
    Payload.startGuarded (Payload.defaultOpts { port = 8080 }) spec { handlers, guards }

getHealthz :: {guards :: {clientip :: Maybe String}} -> Aff String
getHealthz {guards: {clientip}} =
  pure $ "OK " <> fromMaybe "" clientip

getClientIP :: HTTP.Request -> Aff (Maybe String)
getClientIP req =
   pure $ lookup "x-real-ip" hdr
   where
    hdr = HTTP.requestHeaders req

getDraftArticle
  :: Env
  -> { params :: { aptomaId :: String }, query :: DraftParams }
  -> Aff (Response ResponseBody)
getDraftArticle env { params: {aptomaId}, query } = do
  article <- Lettera.getDraftArticle aptomaId query
  renderArticle env article mempty mempty

getArticle
  :: Env
  -> { params :: { uuidOrSlug :: String }, guards :: { clientip :: Maybe String } }
  -> Aff (Response ResponseBody)
getArticle env { params: { uuidOrSlug }, guards: { clientip } }
  | Just uuid <- UUID.parseUUID uuidOrSlug = do
      { article, mostReadArticles, latestArticles } <- sequential $
        { article: _, mostReadArticles: _, latestArticles: _ }
        <$> parallel (Lettera.getArticle uuid mosaicoPaper Nothing clientip)
        <*> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
        <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
      Cache.addHeaderAge 60 <$>
        renderArticle env article mostReadArticles latestArticles
  | otherwise = do
    article <- Lettera.getArticleWithSlug uuidOrSlug mosaicoPaper Nothing clientip
    case article of
      Right a -> do
        pure $ Response
          { status: Status.found
          , body: EmptyBody
          , headers: Headers.fromFoldable [ Tuple "Location" $ "/artikel/" <> a.article.uuid ]
          }
      Left _ -> do
        { mostReadArticles, latestArticles } <- sequential $
          { mostReadArticles: _, latestArticles: _ }
          <$> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
          <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
        let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
            maybeLatest = if null latestArticles then Nothing else Just latestArticles
        notFound env notFoundArticleContent maybeMostRead maybeLatest

renderArticle
  :: Env
  -> Either String FullArticle
  -> Array ArticleStub
  -> Array ArticleStub
  -> Aff (Response ResponseBody)
renderArticle env fullArticle mostReadArticles latestArticles = do
  let mosaico = MosaicoServer.app
      htmlTemplate = cloneTemplate env.htmlTemplate
  case fullArticle of
    Right a@{ article } -> do
      let articleJSX =
            case article.articleType of
              Advertorial
                | elem "Basic" article.categories
                -> Advertorial.Basic.render (Image.render mempty) { article, imageProps: Nothing, advertorialClassName: Nothing }
                | elem "Standard" article.categories -> Advertorial.Standard.render (Image.render mempty) { article }
                | otherwise -> Advertorial.Standard.render (Image.render mempty) { article }
              _ ->
                Article.render (Image.render mempty)
                  { paper: mosaicoPaper
                  , article: Right a
                  , onLogin: mempty
                  , user: Nothing
                  , onPaywallEvent: pure unit
                  , onTagClick: const mempty
                  , onArticleClick: const mempty
                  , mostReadArticles
                  , latestArticles
                  , advertorial: Nothing
                  }
          mosaicoString = DOM.renderToString
                          $ mosaico
                            { mainContent: { type: ArticleContent, content: articleJSX }
                            , mostReadArticles
                            , latestArticles
                            , categoryStructure: env.categoryStructure
                            , user: Nothing
                            }

      html <- liftEffect do
        let windowVars =
              [ "article"           /\ encodeStringifyArticle a.article
              , "articleType"       /\ (show $ show a.articleType)
              , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
              , "latestArticles"    /\ encodeStringifyArticleStubs latestArticles
              , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
              ]
            metaTags =
              let a' = a.article
              in DOM.renderToStaticMarkup $
                  DOM.fragment
                    [ DOM.meta { property: "og:type", content: "article" }
                    , DOM.meta { property: "og:title", content: a'.title }
                    , DOM.meta { property: "og:description", content: fold a'.preamble }
                    , DOM.meta { property: "og:image", content: foldMap _.url a'.mainImage }
                    , DOM.title { children: [ DOM.text a'.title ] }
                    ]
        appendMosaico mosaicoString htmlTemplate >>= appendVars (mkWindowVariables windowVars) >>= appendHead metaTags

      pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
    Left _ ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
          maybeLatest = if null latestArticles then Nothing else Just latestArticles
      in notFound env notFoundArticleContent maybeMostRead maybeLatest

frontpageUpdated :: Env -> { params :: { category :: String }} -> Aff (Response String)
frontpageUpdated env { params: { category } } = do
  Cache.resetCategory env.cache (CategoryLabel category)
  pure $ Response.ok ""

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: { path } } = Handlers.directory "dist/assets" path

frontpage :: Env -> {} -> Aff (Response ResponseBody)
frontpage env {} = do
  prerendered <- Cache.readCategoryRender env.cache frontpageCategoryLabel
  case prerendered of
    Just content -> do
      now <- liftEffect nowDateTime
      pure $ Cache.addHeader now content $ htmlContent $ Response.ok $ StringBody $ Cache.getContent content
    _ -> renderFrontpage env

renderFrontpage :: Env -> Aff (Response ResponseBody)
renderFrontpage env = do
  { articles, mostReadArticles, latestArticles } <- sequential $
    { articles: _, mostReadArticles: _, latestArticles: _ }
    <$> parallel (getFrontpage env.cache frontpageCategoryLabel)
    <*> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent <$> articles <*> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                  , "latestArticles"    /\ encodeStringifyArticleStubs (Cache.getContent latestArticles)
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> mkArticleFeed (CategoryFeed frontpageCategoryLabel) (Cache.getContent articles)
            appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle (Paper.paperName mosaicoPaper))
  let rendered = renderTemplateHtml html
  Cache.saveCategoryRender env.cache frontpageCategoryLabel $ mosaicoString $> rendered
  now <- liftEffect nowDateTime
  pure $ Cache.addHeader now mosaicoString $ htmlContent $ Response.ok $ StringBody rendered
  where
    renderContent articles mostReadArticles latestArticles =
      DOM.renderToString
      $ MosaicoServer.app
          { mainContent: renderFront articles mostReadArticles latestArticles
          , mostReadArticles
          , latestArticles
          , categoryStructure: env.categoryStructure
          , user: Nothing
          }

    renderFront :: ArticleFeed -> Array ArticleStub -> Array ArticleStub -> MainContent
    renderFront (ArticleList list) _ _ =
      { type: FrontpageContent
      , content: Frontpage.render $ Frontpage.List
          { label: mempty
          , content: Just list
          , onArticleClick: const mempty
          , onTagClick: const mempty
          }
      }
    renderFront (Html html) mostReadArticles latestArticles =
      { type: HtmlFrontpageContent
      , content: Frontpage.render $ Frontpage.Prerendered
          { content: Just html
          , hooks: [ Frontpage.RemoveTooltips
                   , Frontpage.MostRead mostReadArticles (const mempty)
                   , Frontpage.Latest latestArticles (const mempty)
                   , Frontpage.ArticleUrltoRelative
                   ]
          , onClick: mempty
          }
      }

getFrontpage :: Cache.Cache -> CategoryLabel -> Aff (Stamped ArticleFeed)
getFrontpage cache category = do
  maybeHtml <- Cache.getFrontpageHtml cache category
  case maybeHtml of
    html@(Stamped {content: Just content}) -> pure $ (const $ Html content) <$> html
    _ -> map ArticleList <$> Cache.getFrontpage cache category

menu :: Env -> {} -> Aff (Response ResponseBody)
menu env _ = do
  let mosaico = MosaicoServer.app
      htmlTemplate = cloneTemplate env.htmlTemplate
  let (emptyRouter :: PushStateInterface) =
        { listen: const $ pure $ pure unit
        , locationState:
            pure
              { hash: mempty
              , path: mempty
              , pathname: mempty
              , search: mempty
              , state: write {}
              }
        , pushState: const $ const mempty
        , replaceState: const $ const mempty
        }
  let mosaicoString =
        DOM.renderToString
        $ mosaico
          { mainContent:
              { type: MenuContent
              , content: Menu.render
                  { categoryStructure: env.categoryStructure
                  , onCategoryClick: const $ handler_ $ pure unit
                  , user: Nothing
                  , onLogin: mempty
                  , onLogout: mempty
                  , router: emptyRouter
                  }
              }
            , mostReadArticles: mempty
            , latestArticles: mempty
            , categoryStructure: env.categoryStructure
            , user: Nothing
          }
  html <- liftEffect do
            let windowVars =
                  [ "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ]
            appendMosaico mosaicoString htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle (Paper.paperName mosaicoPaper))
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

tagList :: Env -> { params :: { tag :: String } } -> Aff (Response ResponseBody)
tagList env { params: { tag } } = do
  let tag' = uriComponentToTag tag
      htmlTemplate = cloneTemplate env.htmlTemplate
  { articles, mostReadArticles, latestArticles } <- sequential $
    { articles: _, mostReadArticles: _, latestArticles: _ }
    <$> parallel (Cache.getByTag env.cache tag')
    <*> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  if null $ Cache.getContent articles
    then notFound
          env
          { type: TagListContent tag', content: notFoundWithAside }
          (Just $ Cache.getContent mostReadArticles)
          (Just $ Cache.getContent latestArticles)
    else do
    let mosaicoString = renderContent tag' <$> articles <*> mostReadArticles <*> latestArticles
    html <- liftEffect do
              let windowVars =
                    [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                    , "latestArticles"    /\ encodeStringifyArticleStubs (Cache.getContent latestArticles)
                    , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                    ] <> mkArticleFeed (TagFeed tag') (ArticleList (Cache.getContent articles))
              appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
                appendVars (mkWindowVariables windowVars) >>=
                appendHead (makeTitle tag)
    now <- liftEffect nowDateTime
    pure $ Cache.addHeader now mosaicoString $
      htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    renderContent tag' articles mostReadArticles latestArticles =
      DOM.renderToString
      $ MosaicoServer.app
      { mainContent:
          { type: TagListContent tag'
          , content: Frontpage.render $ Frontpage.List
              { label: mempty
              , content: Just articles
              , onArticleClick: const mempty
              , onTagClick: const mempty
              }
          }
      , categoryStructure: env.categoryStructure
      , mostReadArticles
      , latestArticles
      , user: Nothing
      }

epaperPage :: Env -> {} -> Aff (Response ResponseBody)
epaperPage env { } = do
  { mostReadArticles, latestArticles } <- sequential $
    { mostReadArticles: _, latestArticles: _ }
    <$> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent <$> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                  , "latestArticles"    /\ encodeStringifyArticleStubs (Cache.getContent latestArticles)
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ]
            appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle "E-Tidningen")
  now <- liftEffect nowDateTime
  pure $ Cache.addHeader now mosaicoString $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    renderContent mostReadArticles latestArticles =
      DOM.renderToString
        $ MosaicoServer.app
          { mainContent:
              { type: EpaperContent
              , content: Epaper.render mempty mosaicoPaper Nothing Nothing
              }
          , categoryStructure: env.categoryStructure
          , mostReadArticles
          , latestArticles
          , user: Nothing
          }


staticPage :: Env -> { params :: { pageName :: String } } -> Aff (Response ResponseBody)
staticPage env { params: { pageName } } = do
  { mostReadArticles, latestArticles } <- sequential $
    { mostReadArticles: _, latestArticles: _ }
    <$> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
    <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
  case HashMap.lookup (pageName <> ".html") env.staticPages of
    Just staticPageContent -> do
      let staticPageScript = HashMap.lookup (pageName <> ".js") env.staticPages
          mosaico = MosaicoServer.app
          htmlTemplate = cloneTemplate env.htmlTemplate
          staticPageJsx =
            DOM.div { className: "mosaico--static-page"
                    , children:
                        [ DOM.div { dangerouslySetInnerHTML: { __html: staticPageContent } }
                        , foldMap (\script -> DOM.script { dangerouslySetInnerHTML: { __html: script } }) staticPageScript
                        ]
                    }
      let mosaicoString =
            DOM.renderToString
            $ mosaico
              { mainContent:
                  { type: StaticPageContent pageName
                  , content: staticPageJsx
                  }
              , mostReadArticles
              , latestArticles
              , categoryStructure: env.categoryStructure
              , user: Nothing
              }
      html <- liftEffect do
        let windowVars =
              [ "staticPageName" /\ (JSON.stringify $ JSON.fromString pageName)
              , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
              ]
        appendMosaico mosaicoString htmlTemplate
          >>= appendVars (mkWindowVariables windowVars)
          >>= appendHead (makeTitle pageName)
      pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
    Nothing ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
          maybeLatest = if null latestArticles then Nothing else Just latestArticles
      in notFound env { type: StaticPageContent pageName, content: notFoundWithAside } maybeMostRead maybeLatest

debugList :: Env -> { params :: { uuid :: String } } -> Aff (Response ResponseBody)
debugList env { params: { uuid } } = do
  { article, mostReadArticles, latestArticles } <- sequential $
    { article: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< map hush <<< Lettera.getArticleStub) (UUID.parseUUID uuid)
    <*> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent article <$> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> mkArticleFeed (CategoryFeed $ CategoryLabel "debug") (ArticleList $ fromFoldable article)
            appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>= appendVars (mkWindowVariables windowVars)
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    renderContent article mostReadArticles latestArticles =
      DOM.renderToString
      $ MosaicoServer.app
          { mainContent:
              { type: FrontpageContent
              , content: Frontpage.render $ Frontpage.List
                  { label: mempty
                  , content: pure <$> article
                  , onArticleClick: const mempty
                  , onTagClick: const mempty
                  }
              }
          , mostReadArticles
          , latestArticles
          , categoryStructure: env.categoryStructure
          , user: Nothing
          }

categoryPage :: Env -> { params :: { categoryName :: String }, guards :: { category :: Category} } -> Aff (Response ResponseBody)
categoryPage env { guards: { category} } = do
  let (Category { label }) = category
  prerendered <- Cache.readCategoryRender env.cache label
  case prerendered of
    Just content -> do
      now <- liftEffect nowDateTime
      pure $ Cache.addHeader now content $ htmlContent $ Response.ok $ StringBody $ Cache.getContent content
    _ -> renderCategoryPage env label

renderCategoryPage :: Env -> CategoryLabel -> Aff (Response ResponseBody)
renderCategoryPage env category = do
  { articles, mostReadArticles, latestArticles } <- sequential $
    { articles: _, mostReadArticles: _, latestArticles: _ }
    <$> parallel (Cache.getFrontpage env.cache category)
    <*> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent <$> articles <*> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                  , "latestArticles"    /\ encodeStringifyArticleStubs (Cache.getContent latestArticles)
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> mkArticleFeed (CategoryFeed category) (ArticleList $ Cache.getContent articles)
            appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle (unwrap category))
  let rendered = renderTemplateHtml html
  Cache.saveCategoryRender env.cache category $ mosaicoString $> rendered
  now <- liftEffect nowDateTime
  pure $ Cache.addHeader now mosaicoString $
    htmlContent $ Response.ok $ StringBody rendered
  where
    renderContent articles mostReadArticles latestArticles =
      DOM.renderToString
      $ MosaicoServer.app
          { mainContent:
              { type: FrontpageContent
              , content: Frontpage.render $ Frontpage.List
                  { label: Just $ unwrap category
                  , content: Just articles
                  , onArticleClick: const mempty
                  , onTagClick: const mempty
                  }
              }
          , mostReadArticles
          , latestArticles
          , categoryStructure: env.categoryStructure
          , user: Nothing
          }

searchPage :: Env -> { query :: { search :: Maybe String } } -> Aff (Response ResponseBody)
searchPage env { query: { search } } = do
  let query = if (trim <$> search) == Just "" then Nothing else search
  searchComponent <- liftEffect Search.searchComponent
  { articles, mostReadArticles, latestArticles } <- sequential $
    { articles: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure mempty) (parallel <<< Lettera.search 0 20 mosaicoPaper) query
    <*> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
    <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
  let mosaico = MosaicoServer.app
      htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = DOM.renderToString
                        $ mosaico
                          { mainContent:
                              { type: FrontpageContent
                              , content:
                                  searchComponent { query
                                                  , doSearch: const $ pure unit
                                                  , searching: false
                                                  , noResults: isJust query && null articles
                                                  } <>
                                  (guard (not $ null articles) $
                                   Frontpage.render $ Frontpage.List
                                   { label: ("Sökresultat: " <> _) <$> query
                                   , content: Just articles
                                   , onArticleClick: const mempty
                                   , onTagClick: const mempty
                                   })
                              }
                          , mostReadArticles
                          , latestArticles
                          , categoryStructure: env.categoryStructure
                          , user: Nothing
                          }
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> mkArticleFeed (SearchFeed $ fromMaybe "" query) (ArticleList articles)
            appendMosaico mosaicoString htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle "Sök")
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

profilePage :: Env -> {} -> Aff (Response ResponseBody)
profilePage env {} = do
  { mostReadArticles, latestArticles } <- sequential $
    { mostReadArticles: _, latestArticles: _ }
    <$> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
    <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = DOM.renderToString
                        $ MosaicoServer.app
                          { mainContent:
                              { type: ProfileContent
                              , content:
                                  Profile.render
                                    { user: Nothing
                                    , onLogin: mempty
                                    , onLogout: mempty
                                    , onStaticPageClick: const mempty
                                    }
                              }
                          , mostReadArticles
                          , latestArticles
                          , categoryStructure: env.categoryStructure
                          , user: Nothing
                          }
  html <- liftEffect do
    let windowVars =
          [ "mostReadArticles" /\ encodeStringifyArticleStubs mostReadArticles
          , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
          ]
    appendMosaico mosaicoString htmlTemplate >>=
      appendVars (mkWindowVariables windowVars) >>=
      appendHead (makeTitle "Min profil")
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

notFoundPage
  :: Env
  -> { params :: { path :: List String } }
  -> Aff (Response ResponseBody)
notFoundPage env {} = notFound env notFoundArticleContent mempty mempty

notFoundArticleContent :: MainContent
notFoundArticleContent =
  { type: ArticleContent
  , content: Article.render (Image.render mempty)
    { paper: mosaicoPaper
    , article: Right notFoundArticle
    , onLogin: mempty
    , user: Nothing
    , onPaywallEvent: pure unit
    , onTagClick: const mempty
    , onArticleClick: const mempty
    , mostReadArticles: mempty
    , latestArticles: mempty
    , advertorial: Nothing
    }
  }

notFound
  :: Env
  -> MainContent
  -> Maybe (Array ArticleStub)
  -> Maybe (Array ArticleStub)
  -> Aff (Response ResponseBody)
notFound env mainContent maybeMostReadArticles maybeLatestArticles = do
  let mosaico = MosaicoServer.app
      htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = DOM.renderToString $ mosaico
                        { mainContent
                        , mostReadArticles: fromMaybe [] maybeMostReadArticles
                        , latestArticles: fromMaybe [] maybeLatestArticles
                        , categoryStructure: env.categoryStructure
                        , user: Nothing
                        }
  html <- liftEffect $ do
    let windowVars =
          [ "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
          ]
          <> foldMap (pure <<< Tuple "mostReadArticles" <<< encodeStringifyArticleStubs) maybeMostReadArticles
          <> foldMap (pure <<< Tuple "latestArticles" <<< encodeStringifyArticleStubs) maybeLatestArticles
          <> (case mainContent.type of
                 ArticleContent -> [ "article" /\ encodeStringifyArticle notFoundArticle.article ]
                 TagListContent tag -> mkArticleFeed (TagFeed tag) (ArticleList [])
                 StaticPageContent pageName -> [ "staticPageName" /\ (JSON.stringify $ JSON.fromString pageName) ]
                 _ -> mempty
             )
    appendMosaico mosaicoString htmlTemplate >>=
      appendVars (mkWindowVariables windowVars) >>=
      appendHead (makeTitle "Oops... 404")
  pure $ htmlContent $ Response.notFound $ StringBody $ renderTemplateHtml html

parseCategory :: Env -> HTTP.Request -> Aff (Either Failure Category)
parseCategory { categoryRegex, categoryStructure } req = do
  let url = HTTP.requestURL req
      urlDecoded = fromMaybe url $ URI.decodeURIComponent url
      categoryRoute = CategoryLabel $ fold $ NonEmptyArray.last =<< Regex.match categoryRegex urlDecoded
      -- Flatten out categories from the category structure
      categories = foldl (\acc (Category c) -> acc <> [Category c] <> c.subCategories) [] categoryStructure
  case find ((_ == categoryRoute) <<< _.label <<< unwrap) categories of
    Just c -> pure $ Right c
    _ -> pure $ Left (Forward "Did not match category")

htmlContent :: forall a. Response a -> Response a
htmlContent (Response response) =
  Response $ response { headers = Headers.set "content-type" ContentType.html response.headers }

mkWindowVariables :: Array (Tuple String String) -> String
mkWindowVariables vars =
  let jsVars = map (\(name /\ value) -> "window." <> name <> "=" <> value <> ";") $
               cons ("globalDisableAds" /\ show globalDisableAds) vars
  in "<script>" <> intercalate "" jsVars <> "</script>"

makeTitle :: String -> String
makeTitle title =
  DOM.renderToStaticMarkup $
    DOM.title { children: [ DOM.text title ] }
