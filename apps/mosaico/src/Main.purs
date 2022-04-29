module Main where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Parallel.Class (parallel, sequential)
import Data.Argonaut.Core as JSON
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find, foldl, fromFoldable, null)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), hush)
import Data.Foldable (fold, foldM, foldMap, elem)
import Data.HashMap as HashMap
import Data.List (List, union, intercalate, (:), snoc)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (trim)
import Data.String.Regex (Regex)
import Data.String.Regex (match, regex) as Regex
import Data.String.Regex.Flags (ignoreCase) as Regex
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign (unsafeToForeign)
import Foreign.Object (lookup)
import JSURI as URI
import KSF.Api (UserAuth, parseToken)
import KSF.Api.Error as Api.Error
import KSF.Paper as Paper
import KSF.User (User, fromPersonaUser, getUserEntitlements)
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
import Mosaico.Paper (mosaicoPaper, _mosaicoPaper)
import Mosaico.Profile as Profile
import Mosaico.Search as Search
import MosaicoServer (MainContent, MainContentType(..))
import MosaicoServer as MosaicoServer
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.FS.Stats as FS
import Node.HTTP as HTTP
import Payload.ContentType as ContentType
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure(..), Response(..), ResponseBody(..))
import Payload.Server as Payload
import Payload.Server.Guards as Guards
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as Handlers
import Payload.Server.Response as Response
import Payload.Server.Status as Status
import Payload.Spec (type (:), GET, Guards, Spec(Spec), Nil)
import Persona as Persona
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
                , guards :: Guards ("credentials" : "clientip" :Nil)
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
                , guards :: Guards ("credentials" : Nil)
                }
         , frontpage ::
              GET "/"
                { response :: ResponseBody
                , guards :: Guards ("credentials" : Nil)
                }
         , menu ::
              GET "/meny"
                { response :: ResponseBody }
         , profilePage ::
              GET "/konto"
                { response :: ResponseBody
                , guards :: Guards ("credentials" : Nil)
                }
         , staticPage ::
              GET "/sida/<pageName>"
                { response :: ResponseBody
                , params :: { pageName :: String }
                , guards :: Guards ("credentials" : Nil)
                }
         , epaperPage ::
              GET "/epaper"
                { response :: ResponseBody
                , guards :: Guards ("credentials" : Nil)
                }
         , debugList ::
              GET "/debug/<uuid>"
                { response :: ResponseBody
                , params :: { uuid :: String }
                , guards :: Guards ("credentials" : Nil)
                }
          , categoryPage ::
              GET "/<categoryName>"
                { response :: ResponseBody
                , params :: { categoryName :: String }
                , guards :: Guards ("credentials" : "category" : Nil)
                }
          , searchPage ::
              GET "/sök?q=<search>"
                { response :: ResponseBody
                , query :: { search :: Maybe String }
                , guards :: Guards ("credentials" : Nil)
                }
          , notFoundPage ::
              GET "/<..path>"
                { response :: ResponseBody
                , params :: { path :: List String}
                , guards :: Guards ("credentials" : Nil)
                }
         }
    , guards ::
         { credentials :: Maybe UserAuth
         , category :: Category
         , clientip :: Maybe String
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
      staticPageNames <- readDir "./static"
      let makeMap acc staticPageFileName = do
            pageContent <- FS.readTextFile UTF8 staticPageFileName
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
          { credentials: getCredentials
          , category: parseCategory env
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
  renderArticle env Nothing article mempty mempty

getArticle
  :: Env
  -> { params :: { uuidOrSlug :: String }, guards :: { credentials :: Maybe UserAuth, clientip :: Maybe String } }
  -> Aff (Response ResponseBody)
getArticle env { params: { uuidOrSlug }, guards: { credentials, clientip } }
  | Just uuid <- UUID.parseUUID uuidOrSlug = do
      { user, article, mostReadArticles, latestArticles } <- sequential $
        { user: _, article: _, mostReadArticles: _, latestArticles: _ }
        <$> maybe (pure Nothing) (parallel <<< getUser) credentials
        <*> parallel (Lettera.getArticle uuid mosaicoPaper credentials clientip)
        <*> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
        <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
      Cache.addHeaderAge 60 (isJust $ hush =<< user) <$>
        renderArticle env user article mostReadArticles latestArticles
  | otherwise = do
    article <- Lettera.getArticleWithSlug uuidOrSlug credentials clientip
    case article of
      Right a -> do
        pure $ Response
          { status: Status.found
          , body: EmptyBody
          , headers: Headers.fromFoldable [ Tuple "Location" $ "/artikel/" <> a.article.uuid ]
          }
      Left _ -> do
        { user, mostReadArticles, latestArticles } <- sequential $
          { user: _, mostReadArticles: _, latestArticles: _ }
          <$> maybe (pure Nothing) (parallel <<< getUser) credentials
          <*> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
          <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
        let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
            maybeLatest = if null latestArticles then Nothing else Just latestArticles
        notFound env (notFoundArticleContent $ hush =<< user) user maybeMostRead maybeLatest

renderArticle
  :: Env
  -> Maybe (Either Unit User)
  -> Either String FullArticle
  -> Array ArticleStub
  -> Array ArticleStub
  -> Aff (Response ResponseBody)
renderArticle env user fullArticle mostReadArticles latestArticles = do
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
                  , user: hush =<< user
                  , onPaywallEvent: pure unit
                  , onTagClick: const mempty
                  , onArticleClick: const mempty
                  , mostReadArticles
                  , latestArticles
                  }
          mosaicoString = DOM.renderToString
                          $ mosaico
                            { mainContent: { type: ArticleContent, content: articleJSX }
                            , mostReadArticles
                            , latestArticles
                            , categoryStructure: env.categoryStructure
                            , user: hush =<< user
                            }

      html <- liftEffect do
        let windowVars =
              [ "article"           /\ encodeStringifyArticle a.article
              , "articleType"       /\ (show $ show a.articleType)
              , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
              , "latestArticles"    /\ encodeStringifyArticleStubs latestArticles
              , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
              ] <> userVar user
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

      pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
    Left _ ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
          maybeLatest = if null latestArticles then Nothing else Just latestArticles
      in notFound env (notFoundArticleContent $ hush =<< user) user maybeMostRead maybeLatest

frontpageUpdated :: Env -> { params :: { category :: String }} -> Aff (Response String)
frontpageUpdated env { params: { category } } = do
  Cache.resetCategory env.cache (CategoryLabel category)
  pure $ Response.ok ""

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: { path } } = Handlers.directory "dist/assets" path

frontpage :: Env -> { guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
frontpage env { guards: { credentials } } = do
  prerendered <- case credentials of
    Nothing -> Cache.readCategoryRender env.cache frontpageCategoryLabel
    Just _ -> pure Nothing
  case prerendered of
    Just content -> do
      now <- liftEffect nowDateTime
      pure $ Cache.addHeader now false content $ htmlContent $ Response.ok $ StringBody $ Cache.getContent content
    _ -> renderFrontpage env credentials

renderFrontpage :: Env -> Maybe UserAuth -> Aff (Response ResponseBody)
renderFrontpage env credentials = do
  { user, articles, mostReadArticles, latestArticles } <- sequential $
    { user: _, articles: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (getFrontpage env.cache frontpageCategoryLabel)
    <*> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent user <$> articles <*> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                  , "latestArticles"    /\ encodeStringifyArticleStubs (Cache.getContent latestArticles)
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> userVar user
                    <> mkArticleFeed (CategoryFeed frontpageCategoryLabel) (Cache.getContent articles)
            appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle (Paper.paperName mosaicoPaper))
  let rendered = renderTemplateHtml html
  when (isNothing user) $
    Cache.saveCategoryRender env.cache frontpageCategoryLabel $ mosaicoString $> rendered
  now <- liftEffect nowDateTime
  pure $ Cache.addHeader now (isJust user) mosaicoString $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody rendered
  where
    renderContent user articles mostReadArticles latestArticles =
      DOM.renderToString
      $ MosaicoServer.app
          { mainContent: renderFront articles mostReadArticles latestArticles
          , mostReadArticles
          , latestArticles
          , categoryStructure: env.categoryStructure
          , user: hush =<< user
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

tagList :: Env -> { params :: { tag :: String }, guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
tagList env { params: { tag }, guards: { credentials } } = do
  let tag' = uriComponentToTag tag
      htmlTemplate = cloneTemplate env.htmlTemplate
  { user, articles, mostReadArticles, latestArticles } <- sequential $
    { user: _, articles: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (Cache.getByTag env.cache tag')
    <*> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  if null $ Cache.getContent articles
    then notFound
          env
          { type: TagListContent tag', content: notFoundWithAside }
          user
          (Just $ Cache.getContent mostReadArticles)
          (Just $ Cache.getContent latestArticles)
    else do
    let mosaicoString = renderContent tag' user <$> articles <*> mostReadArticles <*> latestArticles
    html <- liftEffect do
              let windowVars =
                    [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                    , "latestArticles"    /\ encodeStringifyArticleStubs (Cache.getContent latestArticles)
                    , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                    ] <> userVar user
                      <> mkArticleFeed (TagFeed tag') (ArticleList (Cache.getContent articles))
              appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
                appendVars (mkWindowVariables windowVars) >>=
                appendHead (makeTitle tag)
    now <- liftEffect nowDateTime
    pure $ Cache.addHeader now (isJust user) mosaicoString $
      maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    renderContent tag' user articles mostReadArticles latestArticles =
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
      , user: hush =<< user
      }

epaperPage :: Env -> { guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
epaperPage env { guards: { credentials } } = do
  { user, mostReadArticles, latestArticles } <- sequential $
    { user: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  -- Loading this in parallel with User might make Persona fetch the
  -- user twice from Janrain.  TODO: lift it to make it sequential
  -- inside the parallel above.
  entitlements <- map (fromMaybe Set.empty) $ runMaybeT $ do
    userAuth <- MaybeT $ pure $ (hush =<< user) *> credentials
    MaybeT $ map hush $ getUserEntitlements userAuth
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent user entitlements <$> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                  , "latestArticles"    /\ encodeStringifyArticleStubs (Cache.getContent latestArticles)
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  , "entitlements"      /\ (JSON.stringify $ encodeJson entitlements)
                  ] <> userVar user
            appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle "E-Tidningen")
  now <- liftEffect nowDateTime
  pure $ Cache.addHeader now (isJust user) mosaicoString $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    renderContent user entitlements mostReadArticles latestArticles =
      DOM.renderToString
        $ MosaicoServer.app
          { mainContent:
              -- Having Nothing as entitlements is the loading state
              -- for Epaper component so avoid that for server side
              -- render.
              { type: EpaperContent
              , content: Epaper.render mempty mosaicoPaper ((hush =<< user) *> credentials) (Just entitlements)
              }
          , categoryStructure: env.categoryStructure
          , mostReadArticles
          , latestArticles
          , user: hush =<< user
          }


staticPage :: Env -> { params :: { pageName :: String }, guards :: { credentials :: Maybe UserAuth }} -> Aff (Response ResponseBody)
staticPage env { params: { pageName }, guards: { credentials } } = do
  { user, mostReadArticles, latestArticles } <- sequential $
    { user: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
    <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
  case HashMap.lookup ("./static/" <> _mosaicoPaper <> "/" <> pageName <> ".html") env.staticPages of
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
              , user: hush =<< user
              }
      html <- liftEffect do
        let windowVars =
              [ "staticPageName" /\ (JSON.stringify $ JSON.fromString pageName)
              , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
              ] <> userVar user
        appendMosaico mosaicoString htmlTemplate
          >>= appendVars (mkWindowVariables windowVars)
          >>= appendHead (makeTitle pageName)
      pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
    Nothing ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
          maybeLatest = if null latestArticles then Nothing else Just latestArticles
      in notFound env { type: StaticPageContent pageName, content: notFoundWithAside } user maybeMostRead maybeLatest

debugList :: Env -> { params :: { uuid :: String }, guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
debugList env { params: { uuid }, guards: { credentials } } = do
  { user, article, mostReadArticles, latestArticles } <- sequential $
    { user: _, article: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> maybe (pure Nothing) (parallel <<< map hush <<< Lettera.getArticleStub) (UUID.parseUUID uuid)
    <*> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent user article <$> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> userVar user
                    <> mkArticleFeed (CategoryFeed $ CategoryLabel "debug") (ArticleList $ fromFoldable article)
            appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>= appendVars (mkWindowVariables windowVars)
  pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    renderContent user article mostReadArticles latestArticles =
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
          , user: hush =<< user
          }

categoryPage :: Env -> { params :: { categoryName :: String }, guards :: { category :: Category, credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
categoryPage env { guards: { category, credentials } } = do
  let (Category { label }) = category
  prerendered <- case credentials of
    Nothing -> Cache.readCategoryRender env.cache label
    Just _ -> pure Nothing
  case prerendered of
    Just content -> do
      now <- liftEffect nowDateTime
      pure $ Cache.addHeader now false content $ htmlContent $ Response.ok $ StringBody $ Cache.getContent content
    _ -> renderCategoryPage env label credentials

renderCategoryPage :: Env -> CategoryLabel -> Maybe UserAuth -> Aff (Response ResponseBody)
renderCategoryPage env category credentials = do
  { user, articles, mostReadArticles, latestArticles } <- sequential $
    { user: _, articles: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (Cache.getFrontpage env.cache category)
    <*> parallel (Cache.getMostRead env.cache)
    <*> parallel (Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent user <$> articles <*> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs (Cache.getContent mostReadArticles)
                  , "latestArticles"    /\ encodeStringifyArticleStubs (Cache.getContent latestArticles)
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> userVar user
                    <> mkArticleFeed (CategoryFeed category) (ArticleList $ Cache.getContent articles)
            appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle (unwrap category))
  let rendered = renderTemplateHtml html
  when (isNothing user) $
    Cache.saveCategoryRender env.cache category $ mosaicoString $> rendered
  now <- liftEffect nowDateTime
  pure $ Cache.addHeader now (isJust user) mosaicoString $
    maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody rendered
  where
    renderContent user articles mostReadArticles latestArticles =
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
          , user: hush =<< user
          }

searchPage :: Env -> { query :: { search :: Maybe String }, guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
searchPage env { query: { search }, guards: { credentials } } = do
  let query = if (trim <$> search) == Just "" then Nothing else search
  searchComponent <- liftEffect Search.searchComponent
  { user, articles, mostReadArticles, latestArticles } <- sequential $
    { user: _, articles: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> maybe (pure mempty) (parallel <<< Lettera.search 0 20 mosaicoPaper) query
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
                          , user: hush =<< user
                          }
  html <- liftEffect do
            let windowVars =
                  [ "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> userVar user
                    <> mkArticleFeed (SearchFeed $ fromMaybe "" query) (ArticleList articles)
            appendMosaico mosaicoString htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle "Sök")
  pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

profilePage :: Env -> { guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
profilePage env { guards: { credentials }} = do
  { user, mostReadArticles, latestArticles } <- sequential $
    { user: _, mostReadArticles: _, latestArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
    <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = DOM.renderToString
                        $ MosaicoServer.app
                          { mainContent:
                              { type: ProfileContent
                              , content:
                                  Profile.render
                                    { user: hush =<< user
                                    , onLogin: mempty
                                    , onLogout: mempty
                                    , onStaticPageClick: const mempty
                                    }
                              }
                          , mostReadArticles
                          , latestArticles
                          , categoryStructure: env.categoryStructure
                          , user: hush =<< user
                          }
  html <- liftEffect do
    let windowVars =
          [ "mostReadArticles" /\ encodeStringifyArticleStubs mostReadArticles
          , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
          ] <> userVar user
    appendMosaico mosaicoString htmlTemplate >>=
      appendVars (mkWindowVariables windowVars) >>=
      appendHead (makeTitle "Min profil")
  pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

notFoundPage
  :: Env
  -> { params :: { path :: List String }, guards :: { credentials :: Maybe UserAuth } }
  -> Aff (Response ResponseBody)
notFoundPage env { guards: { credentials } } = do
  user <- maybe (pure Nothing) getUser credentials
  notFound env (notFoundArticleContent $ hush =<< user) user mempty mempty

notFoundArticleContent :: Maybe User -> MainContent
notFoundArticleContent user =
  { type: ArticleContent
  , content: Article.render (Image.render mempty)
    { paper: mosaicoPaper
    , article: Right notFoundArticle
    , onLogin: mempty
    , user
    , onPaywallEvent: pure unit
    , onTagClick: const mempty
    , onArticleClick: const mempty
    , mostReadArticles: mempty
    , latestArticles: mempty
    }
  }

notFound
  :: Env
  -> MainContent
  -> Maybe (Either Unit User)
  -> Maybe (Array ArticleStub)
  -> Maybe (Array ArticleStub)
  -> Aff (Response ResponseBody)
notFound env mainContent user maybeMostReadArticles maybeLatestArticles = do
  let mosaico = MosaicoServer.app
      htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = DOM.renderToString $ mosaico
                        { mainContent
                        , mostReadArticles: fromMaybe [] maybeMostReadArticles
                        , latestArticles: fromMaybe [] maybeLatestArticles
                        , categoryStructure: env.categoryStructure
                        , user: hush =<< user
                        }
  html <- liftEffect $ do
    let windowVars =
          [ "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
          ] <> userVar user
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
  pure $ maybeInvalidateAuth user $ htmlContent $ Response.notFound $ StringBody $ renderTemplateHtml html

getCredentials :: HTTP.Request -> Aff (Maybe UserAuth)
getCredentials req = do
  cookies <- Guards.cookies req
  pure do
    authToken <- parseToken =<< Map.lookup "Authorization" cookies
    userId <- UUID.parseUUID =<< Map.lookup "AuthUser" cookies
    pure { authToken, userId }

-- Nothing if not set, Left when we need to invalidate
getUser :: UserAuth -> Aff (Maybe (Either Unit User))
getUser auth = do
  response <- try $ Persona.getUser Nothing auth.userId auth
  pure $ case response of
    Right user -> Just $ Right $ fromPersonaUser user
    Left err
      | Just (errData :: Persona.TokenInvalid) <- Api.Error.errorData err -> Just $ Left unit
      | otherwise -> Nothing

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

-- TODO: invalidate Authorization too
maybeInvalidateAuth :: forall a. Maybe (Either Unit User) -> Response a -> Response a
maybeInvalidateAuth (Just (Left _)) (Response response) =
  Response $ response { headers = Headers.set "set-cookie" "AuthUser=; Path=/; Max-age=0" response.headers }
maybeInvalidateAuth _ response = response

mkWindowVariables :: Array (Tuple String String) -> String
mkWindowVariables vars =
  let jsVars = map (\(name /\ value) -> "window." <> name <> "=" <> value <> ";") vars
  in "<script>" <> intercalate "" jsVars <> "</script>"

userVar :: Maybe (Either Unit User) -> Array (Tuple String String)
userVar = (_ >>= hush) >>>
          foldMap (pure <<< Tuple "user" <<< Persona.rawJSONStringify <<< unsafeToForeign)

makeTitle :: String -> String
makeTitle title =
  DOM.renderToStaticMarkup $
    DOM.title { children: [ DOM.text title ] }
