module Main where

import Prelude

import Control.Parallel.Class (parallel, sequential)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as JSON
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Array (cons, find, foldl, fromFoldable, head, null)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), hush)
import Data.Foldable (fold, foldM, foldMap, elem)
import Data.HashMap as HashMap
import Data.Map as Map
import Data.Map (Map)
import Data.List (List (..), union, intercalate, (:), snoc)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String (trim)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex (match, regex, test) as Regex
import Data.String.Regex.Flags (ignoreCase, noFlags) as Regex
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
import Foreign.Object (Object, lookup)
import JSURI as URI
import KSF.Paper as Paper
import KSF.Random (randomString)
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category(..), CategoryLabel(..), CategoryType(..), DraftParams, FullArticle, ArticleType(..), articleToJson, articleStubToJson, frontpageCategoryLabel, notFoundArticle, uriComponentToTag)
import Lettera.ArticleSchema (renderAsJsonLd)
import Mosaico.Article as Article
import Mosaico.Article.Advertorial.Basic as Advertorial.Basic
import Mosaico.Article.Advertorial.Standard as Advertorial.Standard
import Mosaico.Article.Box as Box
import Mosaico.Article.Image as Image
import Mosaico.Cache (Stamped, parallelWithCommonLists)
import Mosaico.Cache as Cache
import Mosaico.Epaper as Epaper
import Mosaico.Error (notFoundWithAside)
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), mkArticleFeed)
import Mosaico.Frontpage (Frontpage(..), render) as Frontpage
import Mosaico.Frontpage.Models (Hook(..)) as Frontpage
import Mosaico.Header.Menu as Menu
import Mosaico.Paper (mosaicoPaper, _mosaicoPaper)
import Mosaico.Profile as Profile
import Mosaico.Search as Search
import Mosaico.Webview as Webview
import MosaicoServer (MainContent, MainContentType(..))
import MosaicoServer as MosaicoServer
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, stat) as FS
import Node.FS.Stats (isFile) as FS
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
import React.Basic (JSX)
import React.Basic (fragment) as DOM
import React.Basic.DOM (div, meta, script, text, title) as DOM
import React.Basic.DOM.Server (renderToStaticMarkup, renderToString) as DOM
import React.Basic.Events (handler_)
import Simple.JSON (readJSON)

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
  , redirects :: Map (Tuple String Paper.Paper) String
  }

type Redirect =
  { paper :: Maybe Paper.Paper
  , route :: String
  , destination :: String
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
         , googleSiteVerification ::
              GET "/google8c22fe93f3684c84.html"
                { response :: File }
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
              GET "/artikel/<..uuidOrSlug>"
                { response :: ResponseBody
                , params :: { uuidOrSlug :: List String }
                , guards :: Guards ("clientip" : Nil)
                }
         , adsTxt ::
              GET "/ads.txt"
                { response :: File }
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
              GET "/epaper/<..path>?<..query>"
                { response :: ResponseBody
                , params :: { path :: List String }
                , query :: { query :: Object (Array String) }
                , guards :: Guards ("epaper" : Nil)
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
         , epaper :: Unit
         }
    }
spec = Spec

readDir :: String -> Effect (List String)
readDir dir = do
  let go :: List String -> List String -> Effect (List String)
      go acc List.Nil = pure acc
      go acc (c : tail) = do
        let fullFilePath = dir <> "/" <> c
        fileStats <- FS.stat fullFilePath
        if FS.isFile fileStats
        then go (acc `snoc` fullFilePath) tail
        else do
           contents <- readDir fullFilePath
           go (acc `union` contents) tail

  contents <- FS.readdir dir
  go mempty $ List.fromFoldable contents

main :: Effect Unit
main = do
  staticPages  <- do
      let pageMatch =
            maybe (const false) Regex.test $ hush $
            Regex.regex ("^\\./dist/static/(" <> _mosaicoPaper <> "/[^/]+\\.html|[^/]+\\.js)$")
            Regex.noFlags
      staticPageNames <- List.filter pageMatch <$> readDir "./dist/static"
      let makeMap acc staticPagePath = do
            case Array.last $ String.split (String.Pattern "/") staticPagePath of
              Just fileName -> do
                pageContent <- FS.readTextFile UTF8 staticPagePath
                pure $ HashMap.insert fileName pageContent acc
              _ -> pure acc
      foldM makeMap HashMap.empty staticPageNames
  htmlTemplate <- parseTemplate <$> FS.readTextFile UTF8 indexHtmlFileLocation
  Aff.launchAff_ do
    categoryStructure <- Lettera.getCategoryStructure mosaicoPaper
    cache <- liftEffect $ Cache.initServerCache categoryStructure
    -- This is used for matching a category label from a route, such as "/nyheter" or "/norden-och-världen"
    categoryRegex <- case Regex.regex "^\\/([\\w|ä|ö|å|-]+)\\b" Regex.ignoreCase of
      Right r -> pure r
      Left _  -> liftEffect $ throw "I have a very safe regex to parse, yet somehow I didn't know how to parse it. Fix it please. Exploding now, goodbye."
    redirects <- liftEffect do
      -- The redirects are read from a JSON file for now,
      -- but it's easy to change this to read whatever
      -- bucket or source we want to have it in the future
      redirJson <- FS.readTextFile UTF8 "./dist/redir.json"
      case readJSON redirJson of
        Right (redirs :: Array Redirect) ->
          let mkRedir acc { route, paper, destination } =
                case paper of
                  Just p -> Map.insert (route /\ p) destination acc
                  -- If it's paper agnostic, let's create
                  -- a redirect rule for all our three mosaico papers
                  Nothing ->
                    Map.empty
                    # Map.insert (route /\ Paper.HBL) destination
                    # Map.insert (route /\ Paper.VN)  destination
                    # Map.insert (route /\ Paper.ON)  destination
                    # Map.union acc
          in pure $ foldl mkRedir Map.empty redirs
        Left _err -> throw "Could not parse redir.json! Check the file and try again"
    let env = { htmlTemplate, categoryStructure, categoryRegex, staticPages, cache, redirects }
        handlers =
          { getHealthz
          , googleSiteVerification
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
          , adsTxt
          }
        guards =
          { category: parseCategory env
          , clientip: getClientIP
          , epaper: epaperGuard
          }
    void $ Payload.startGuarded (Payload.defaultOpts { port = 8080 }) spec { handlers, guards }
    pure unit

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
  -> { params :: { uuidOrSlug :: List String }, guards :: { clientip :: Maybe String } }
  -> Aff (Response ResponseBody)
getArticle env { params: { uuidOrSlug: (uuid : _) }, guards: { clientip } }
  | Just articleId <- UUID.parseUUID uuid = do
      { pageContent: article, mostReadArticles, latestArticles } <-
        parallelWithCommonLists env.cache $ Lettera.getArticle articleId mosaicoPaper Nothing clientip
      Cache.addHeaderAge 60 <$>
        renderArticle env article (Cache.getContent mostReadArticles) (Cache.getContent latestArticles)
getArticle env { params: { uuidOrSlug: path }, guards: { clientip } }
  | (slug : _) <- path
  , not $ String.null slug
  = do
      article <- Lettera.getArticleWithSlug slug mosaicoPaper Nothing clientip
      case article of
        Right a -> do
          pure $ Response
            { status: Status.found
            , body: EmptyBody
            , headers: Headers.fromFoldable [ Tuple "Location" $ "/artikel/" <> a.article.uuid ]
            }
        Left _ -> renderNotFound env
   | otherwise = renderNotFound env

renderNotFound :: Env -> Aff (Response ResponseBody)
renderNotFound env = do
  feeds <- sequential $
    { mostReadArticles: _, latestArticles: _ }
    <$> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
    <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
  let maybeMostRead = if null feeds.mostReadArticles then Nothing else Just feeds.mostReadArticles
      maybeLatest   = if null feeds.latestArticles then Nothing else Just feeds.latestArticles
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
                -> renderWithComponents Advertorial.Basic.render { article, imageProps: Nothing, advertorialClassName: Nothing }
                | elem "Standard" article.categories -> renderWithComponents Advertorial.Standard.render { article }
                | otherwise -> renderWithComponents Advertorial.Standard.render { article }
              _ ->
                renderWithComponents Article.render
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
                  , breakingNews: mempty
                  }
          renderWithComponents :: forall a. ((Image.Props -> JSX) -> (Box.Props -> JSX) -> a -> JSX) -> a -> JSX
          renderWithComponents f = f (Image.render mempty) (Box.render mempty)
          mosaicoString = DOM.renderToString
                          $ mosaico
                            { mainContent: { type: ArticleContent, content: articleJSX }
                            , mostReadArticles
                            , latestArticles
                            , categoryStructure: env.categoryStructure
                            }

      html <- liftEffect do
        let windowVars =
              [ "article"           /\ (encodeJson $ articleToJson a.article)
              , "articleType"       /\ (JSON.fromString $ show a.articleType)
              , "mostReadArticles"  /\ (encodeJson $ map articleStubToJson mostReadArticles)
              , "latestArticles"    /\ (JSON.fromArray $ map articleStubToJson latestArticles)
              , "categoryStructure" /\ encodeJson env.categoryStructure
              , "breakingNews"      /\ JSON.jsonNull
              ]
            metaTags =
              let a' = a.article
              in DOM.renderToStaticMarkup $
                  DOM.fragment
                    [ DOM.meta { property: "og:type", content: "article" }
                    , DOM.meta { property: "og:title", content: a'.title }
                    , DOM.meta { property: "og:description", content: fold a'.preamble }
                    , foldMap (\url -> DOM.meta { property: "og:image", content: url}) $ _.url <$> a'.mainImage
                    , DOM.meta { name: "description", content: fold a'.preamble }
                    , foldMap (const $ DOM.meta { name: "robots", content: "max-image-preview:large"}) a'.mainImage
                    , DOM.title { children: [ DOM.text a'.title ] }
                    , DOM.script
                        { type: "application/ld+json"
                        , dangerouslySetInnerHTML:
                            { __html:
                                String.replaceAll (String.Pattern "<") (String.Replacement "\\u003c")
                                  $ JSON.stringify
                                  $ renderAsJsonLd a'
                            }
                        }
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

adsTxt :: forall r. { | r} -> Aff File
adsTxt = Handlers.file "dist/assets/ads.txt"

googleSiteVerification :: forall r. { | r} -> Aff File
googleSiteVerification = Handlers.file "dist/assets/google8c22fe93f3684c84.html"

frontpage :: Env -> {} -> Aff (Response ResponseBody)
frontpage env {} = do
  prerendered <- Cache.readCategoryRender env.cache frontpageCategoryLabel
  case prerendered of
    Just content -> do
      now <- liftEffect nowDateTime
      pure $ Cache.addHeader now content $ htmlContent $ Response.ok $ StringBody $ Cache.getContent content
    _ -> case head env.categoryStructure of
      Just frontpageCategory -> renderCategoryPage env frontpageCategory
      _ -> pure $ Response.internalError $ StringBody "no categorystructure defined"

menu :: Env -> {} -> Aff (Response ResponseBody)
menu env _ = do
  let mosaico = MosaicoServer.app
      htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString =
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
                  , changeRoute: const mempty
                  }
              }
            , mostReadArticles: mempty
            , latestArticles: mempty
            , categoryStructure: env.categoryStructure
          }
  html <- liftEffect do
            let windowVars =
                  [ "categoryStructure" /\ encodeJson env.categoryStructure
                  ]
            appendMosaico mosaicoString htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle (Paper.paperName mosaicoPaper))
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

tagList :: Env -> { params :: { tag :: String } } -> Aff (Response ResponseBody)
tagList env { params: { tag } } = do
  let tag' = uriComponentToTag tag
      htmlTemplate = cloneTemplate env.htmlTemplate
  { pageContent: articles, breakingNews, mostReadArticles, latestArticles } <-
    parallelWithCommonLists env.cache $ Cache.getByTag env.cache tag'
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
                    stdVars env (Just breakingNews) mostReadArticles latestArticles
                    <> mkArticleFeed (TagFeed tag') (ArticleList (Cache.getContent articles))
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
      }

epaperGuard :: HTTP.Request -> Aff (Either Failure Unit)
epaperGuard req = do
  let url = HTTP.requestURL req
  pure $ if url == "/epaper/" || String.take 9 url == "/epaper/?"
         then Right unit else Left (Forward "not exact match with epaper page")

epaperPage :: Env -> { params :: { path :: List String }, query :: { query :: Object (Array String) }, guards :: { epaper :: Unit } } -> Aff (Response ResponseBody)
epaperPage env {} = do
  { mostReadArticles, latestArticles } <- parallelWithCommonLists env.cache $ pure unit
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent <$> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  stdVars env Nothing mostReadArticles latestArticles
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
              , content: Epaper.render mempty mosaicoPaper true Nothing Nothing
              }
          , categoryStructure: env.categoryStructure
          , mostReadArticles
          , latestArticles
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
              }
      html <- liftEffect do
        let windowVars =
              [ "staticPageName" /\ JSON.fromString pageName
              , "categoryStructure" /\ encodeJson env.categoryStructure
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
  { pageContent: article, mostReadArticles, latestArticles } <-
    parallelWithCommonLists env.cache $
    maybe (pure Nothing) (map hush <<< Lettera.getArticleStub) (UUID.parseUUID uuid)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent article <$> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  stdVars env Nothing mostReadArticles latestArticles
                  <> mkArticleFeed (CategoryFeed $ CategoryLabel "debug") (ArticleList $ fromFoldable article)
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
          }

categoryPage :: Env -> { params :: { categoryName :: String }, guards :: { category :: Category} } -> Aff (Response ResponseBody)
categoryPage env { guards: { category: category@(Category{label})}} = do
  prerendered <- Cache.readCategoryRender env.cache label
  case prerendered of
    Just content -> do
      now <- liftEffect nowDateTime
      pure $ Cache.addHeader now content $ htmlContent $ Response.ok $ StringBody $ Cache.getContent content
    _ -> renderCategoryPage env category

renderCategoryPage :: Env -> Category -> Aff (Response ResponseBody)
renderCategoryPage env (Category category@{ label, type: categoryType, url}) = do
  { feed, mainContent, breakingNews, mostReadArticles, latestArticles } <- do
    case categoryType of
      Feed -> do
        { pageContent, breakingNews, mostReadArticles, latestArticles } <-
          parallelWithCommonLists env.cache $ Cache.getFrontpage env.cache label
        pure { feed: Just $ ArticleList $ Cache.getContent pageContent
             , mainContent:
                 (\articles ->
                     { type: FrontpageContent
                     , content: Frontpage.render $ Frontpage.List
                                  { label: Just $ unwrap label
                                  , content: Just articles
                                  , onArticleClick: const mempty
                                  , onTagClick: const mempty
                                  }
                     }) <$> pageContent
             , mostReadArticles
             , latestArticles
             , breakingNews
             }
      Prerendered -> do
        { pageContent, breakingNews, mostReadArticles, latestArticles, articleStubs } <- sequential $
          { pageContent: _, breakingNews: _, mostReadArticles: _, latestArticles: _, articleStubs: _ }
          <$> parallel (Cache.getFrontpageHtml env.cache label)
          <*> parallel (Cache.getBreakingNewsHtml env.cache)
          <*> parallel (Cache.getMostRead env.cache)
          <*> parallel (Cache.getLatest env.cache)
          <*> parallel (Cache.getFrontpage env.cache label)
        let hooks = [ Frontpage.RemoveTooltips
                    , Frontpage.MostRead (Cache.getContent mostReadArticles) (const mempty)
                    , Frontpage.Latest (Cache.getContent latestArticles) (const mempty)
                    , Frontpage.ArticleUrltoRelative
                    , Frontpage.EpaperBanner
                    ]
        pure { feed: Html (Cache.getContent articleStubs) <$> Cache.getContent pageContent
             , mainContent: articleStubs *>
                 ((\html ->
                     { type: HtmlFrontpageContent
                     , content: Frontpage.render $ Frontpage.Prerendered
                                  { content: html
                                  , breakingNews: Cache.getContent breakingNews
                                  , hooks
                                  , onClick: mempty
                                  }
                     }) <$> pageContent)
             , mostReadArticles
             , latestArticles
             , breakingNews
             }
      Link -> do
        { mostReadArticles, latestArticles, breakingNews } <- parallelWithCommonLists env.cache $ pure unit
        pure { feed: Nothing
             , mainContent: mostReadArticles *> latestArticles $>
                            { type: StaticPageContent "link"
                            -- TODO
                            , content: mempty
                            }
             , mostReadArticles
             , latestArticles
             , breakingNews
             }
      Webview -> do
        initialRandom <- liftEffect $ randomString 10
        { mostReadArticles, latestArticles, breakingNews } <- parallelWithCommonLists env.cache $ pure unit
        -- video.js fails if it tries to initialize an M3U8 stream for a second time.
        -- If it had the webview component rendered on server side, React's hydrate
        -- would count as a second initialization.
        let streamURL = Webview.streamURL <$> url
            streamType = streamURL >>= Webview.parseStreamType
            content = case streamType of
              Just Webview.M3U8 -> DOM.div
                                     { className: "mosaico-webview"
                                     , children: [ DOM.text "Laddar..." ]
                                     }
              _                 -> Webview.render streamType (fromMaybe "" streamURL) initialRandom
            mainContent = mostReadArticles *> latestArticles $>
                          { type: WebviewContent
                          , content
                          }
        pure { feed: Nothing
             , mainContent
             , mostReadArticles
             , latestArticles
             , breakingNews
             }

  -- Fallback to using list feed style
  let fallbackToList = renderCategoryPage env $ Category $ category { type = Feed }
  case categoryType of
    Prerendered
      | Nothing <- feed -> fallbackToList
      -- Sanity check
      | Just (Html _ html) <- feed
      , Right regex <- emptyRegex
      , Just _ <- Regex.match regex html -> fallbackToList
    _ -> do
      let htmlTemplate = cloneTemplate env.htmlTemplate
          mosaicoString = renderContent <$> mainContent <*> mostReadArticles <*> latestArticles
          windowVars = stdVars env (Just breakingNews) mostReadArticles latestArticles
                       <> foldMap (mkArticleFeed $ CategoryFeed label) feed
      html <- liftEffect $ appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              maybe pure appendHead (guard (label == frontpageCategoryLabel) $ Just startpageMeta) >>=
              appendHead (makeTitle title)
      let rendered = renderTemplateHtml html
      Cache.saveCategoryRender env.cache label $ mosaicoString $> rendered
      now <- liftEffect nowDateTime
      pure $ Cache.addHeader now mosaicoString $
        htmlContent $ Response.ok $ StringBody rendered
  where
    emptyRegex = Regex.regex "^\\s*$" Regex.noFlags
    renderContent mainContent mostReadArticles latestArticles =
      DOM.renderToString
      $ MosaicoServer.app
          { mainContent
          , mostReadArticles
          , latestArticles
          , categoryStructure: env.categoryStructure
          }
    title = if label == frontpageCategoryLabel then Paper.paperName mosaicoPaper else unwrap label
    startpageDescription = case mosaicoPaper of
      Paper.HBL -> Just "En sajt om samtiden för dig som vill uppleva, delta och påverka – på svenska."
      Paper.ON -> Just "Nyheter från östra Nylands största svenskspråkiga tidning."
      Paper.VN -> Just "Nyheter från Västnylands största svenskspråkiga tidning."
      _ -> Nothing
    startpageMeta = DOM.renderToStaticMarkup $
      DOM.fragment
        [ DOM.meta { property: "og:type", content: "website" }
        , DOM.meta { property: "og:title", content: title }
        , foldMap (\content -> DOM.meta { property: "og:description", content }) startpageDescription
        , foldMap (\content -> DOM.meta { name: "description", content }) startpageDescription
        ]

searchPage :: Env -> { query :: { search :: Maybe String } } -> Aff (Response ResponseBody)
searchPage env { query: { search } } = do
  let query = if (trim <$> search) == Just "" then Nothing else search
  searchComponent <- liftEffect Search.searchComponent
  { pageContent: articles, mostReadArticles, latestArticles } <-
    parallelWithCommonLists env.cache $
    maybe (pure mempty) (pure <<< join <<< fromFoldable <=< Lettera.search 0 20 mosaicoPaper) query
  let mosaico = MosaicoServer.app
      htmlTemplate = cloneTemplate env.htmlTemplate
      noResults = isJust query && null articles
      mosaicoString = DOM.renderToString
                        $ mosaico
                          { mainContent:
                              { type: FrontpageContent
                              , content:
                                  searchComponent { query
                                                  , doSearch: const $ pure unit
                                                  , searching: false
                                                  } <>
                                  (guard (not $ null articles) $
                                   Frontpage.render $ Frontpage.List
                                   { label: if noResults
                                            then Just "Inga resultat"
                                            else ("Sökresultat: " <> _) <$> query
                                   , content: Just articles
                                   , onArticleClick: const mempty
                                   , onTagClick: const mempty
                                   })
                              }
                          , mostReadArticles: Cache.getContent mostReadArticles
                          , latestArticles: Cache.getContent latestArticles
                          , categoryStructure: env.categoryStructure
                          }
  html <- liftEffect do
            let windowVars =
                  stdVars env Nothing mostReadArticles latestArticles
                  <> mkArticleFeed (SearchFeed $ fromMaybe "" query) (ArticleList articles)
            appendMosaico mosaicoString htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle "Sök")
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

profilePage :: Env -> {} -> Aff (Response ResponseBody)
profilePage env {} = do
  { mostReadArticles, latestArticles } <- parallelWithCommonLists env.cache $ pure unit
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
                          , mostReadArticles: Cache.getContent mostReadArticles
                          , latestArticles: Cache.getContent latestArticles
                          , categoryStructure: env.categoryStructure
                          }
  html <- liftEffect do
    let windowVars =
          stdVars env Nothing mostReadArticles latestArticles
    appendMosaico mosaicoString htmlTemplate >>=
      appendVars (mkWindowVariables windowVars) >>=
      appendHead (makeTitle "Min profil")
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

notFoundPage
  :: Env
  -> { params :: { path :: List String } }
  -> Aff (Response ResponseBody)
notFoundPage env { params: { path } } = do
  -- TODO move redirect logic behind its own guard and route
  let redir to = pure $
        (\(Response r) -> Response $ r { headers = Headers.set "Location" to r.headers }) $
        Response.found EmptyBody
      pass = notFound env notFoundArticleContent mempty mempty
  case path of
    (route : Nil)      | Just destination <- Map.lookup (route /\ mosaicoPaper) env.redirects -> redir destination
    -- Same route with trailing slash
    (route : "" : Nil) | Just destination <- Map.lookup (route /\ mosaicoPaper) env.redirects -> redir destination
    _                                                                                         -> pass

notFoundArticleContent :: MainContent
notFoundArticleContent =
  { type: ArticleContent
  , content: Article.render (Image.render mempty) (Box.render mempty)
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
    , breakingNews: mempty
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
                        }
  html <- liftEffect $ do
    let windowVars =
          [ "categoryStructure" /\ encodeJson env.categoryStructure
          , "breakingNews" /\ (encodeJson $ JSON.jsonNull)
          ]
          <> foldMap (pure <<< Tuple "mostReadArticles" <<< JSON.fromArray <<< map articleStubToJson) maybeMostReadArticles
          <> foldMap (pure <<< Tuple "latestArticles" <<< JSON.fromArray <<< map articleStubToJson) maybeLatestArticles
          <> (case mainContent.type of
                 ArticleContent -> [ "article" /\ (encodeJson $ articleToJson notFoundArticle.article) ]
                 TagListContent tag -> mkArticleFeed (TagFeed tag) (ArticleList [])
                 StaticPageContent pageName -> [ "staticPageName" /\ JSON.fromString pageName ]
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

stdVars :: Env -> Maybe (Stamped String) -> Stamped (Array ArticleStub) -> Stamped (Array ArticleStub) -> Array (Tuple String Json)
stdVars env initialBreakingNews mostReadArticles latestArticles =
  [ "mostReadArticles"  /\ JSON.fromArray (map articleStubToJson (Cache.getContent mostReadArticles))
  , "latestArticles"    /\ JSON.fromArray (map articleStubToJson (Cache.getContent latestArticles))
  , "categoryStructure" /\ encodeJson env.categoryStructure
  , "breakingNews"      /\ maybe JSON.jsonNull JSON.fromString (Cache.getContent <$> initialBreakingNews)
  ]

mkWindowVariables :: Array (Tuple String Json) -> String
mkWindowVariables vars =
  let jsVars = map (\(name /\ value) -> "window." <> name <> "=" <> stringify value <> ";") $
               cons ("globalDisableAds" /\ JSON.fromBoolean globalDisableAds) vars
      stringify =
        String.replaceAll (String.Pattern "<") (String.Replacement "\\u003c") <<< JSON.stringify
  in "<script>" <> intercalate "" jsVars <> "</script>"

makeTitle :: String -> String
makeTitle title =
  DOM.renderToStaticMarkup $
    DOM.title { children: [ DOM.text title ] }
