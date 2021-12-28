module Main where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Parallel.Class (parallel, sequential)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Core as JSON
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find, foldl, fromFoldable, null)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), hush)
import Data.Foldable (fold, foldM, foldMap)
import Data.HashMap as HashMap
import Data.List (List, intercalate)
import Data.Map as Map
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
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign (unsafeToForeign)
import JSURI as URI
import KSF.Api (UserAuth, parseToken)
import KSF.Api.Error as Api.Error
import KSF.Paper (Paper)
import KSF.User (User, fromPersonaUser)
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category(..), CategoryLabel(..), DraftParams, FullArticle, encodeStringifyArticle, encodeStringifyArticleStubs, fromFullArticle, isDraftArticle, isPreviewArticle, notFoundArticle, uriComponentToTag)
import Mosaico.Article as Article
import Mosaico.Error (notFoundWithAside)
import Mosaico.Frontpage as Frontpage
import Mosaico.Frontpage.Models as Frontpage
import Mosaico.Header.Menu as Menu
import Mosaico.Models (ArticleFeed(..))
import Mosaico.MostReadList as MostReadList
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Search as Search
import MosaicoServer (MainContent(..))
import MosaicoServer as MosaicoServer
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
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
import React.Basic (JSX)
import React.Basic (fragment) as DOM
import React.Basic.DOM (div, meta, script) as DOM
import React.Basic.DOM.Server (renderToStaticMarkup, renderToString) as DOM
import React.Basic.Events (handler_)
import Routing.PushState (PushStateInterface)
import Simple.JSON (write)

foreign import appendMosaicoImpl :: EffectFn2 String String String
appendMosaico :: String -> String -> Effect String
appendMosaico content htmlTemplate = runEffectFn2 appendMosaicoImpl content htmlTemplate

foreign import appendHeadImpl :: EffectFn2 String String String
appendHead :: String -> String -> Effect String
appendHead = runEffectFn2 appendHeadImpl

foreign import serverPort :: Int

type Env =
  { htmlTemplate :: String
  , categoryStructure :: Array Category
  , categoryRegex :: Regex
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
         , staticPage ::
              GET "/sida/<pageName>"
                { response :: ResponseBody
                , params :: { pageName :: String }
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
  htmlTemplate <- FS.readTextFile UTF8 indexHtmlFileLocation
  Aff.launchAff_ do
    categoryStructure <- Lettera.getCategoryStructure mosaicoPaper
    -- This is used for matching a category label from a route, such as "/nyheter" or "/norden-och-världen"
    categoryRegex <- case Regex.regex "^\\/([\\w|ä|ö|å|-]+)\\b" Regex.ignoreCase of
      Right r -> pure r
      Left _  -> liftEffect $ throw "I have a very safe regex to parse, yet somehow I didn't know how to parse it. Fix it please. Exploding now, goodbye."
    let env = { htmlTemplate, categoryStructure, categoryRegex, staticPages }
        handlers =
          { getDraftArticle: getDraftArticle env
          , getArticle: getArticle env
          , assets
          , frontpage: frontpage env
          , tagList: tagList env
          , staticPage: staticPage env
          , debugList: debugList env
          , categoryPage: categoryPage env
          , searchPage: searchPage env
          , notFoundPage: notFoundPage env
          , menu: menu env
          }
        guards = { credentials: getCredentials, category: parseCategory env }
    Payload.startGuarded (Payload.defaultOpts { port = 8080 }) spec { handlers, guards }

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
getArticle env r@{ params: { uuidOrSlug }, guards: { credentials } }
  | Just uuid <- UUID.parseUUID uuidOrSlug = do
      { user, article, mostReadArticles } <- sequential $
        { user: _, article: _, mostReadArticles: _ }
        <$> maybe (pure Nothing) (parallel <<< getUser) credentials
        <*> parallel (Lettera.getArticle uuid r.guards.credentials)
        <*> parallel (Lettera.getMostRead 0 10 Nothing mosaicoPaper true)
      renderArticle env user article mostReadArticles
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
        { user, mostReadArticles } <- sequential $
          { user: _, mostReadArticles: _ }
          <$> maybe (pure Nothing) (parallel <<< getUser) credentials
          <*> parallel (Lettera.getMostRead 0 10 Nothing mosaicoPaper true)
        let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
        notFound env (notFoundArticleContent $ hush =<< user) user maybeMostRead

renderArticle
  :: Env
  -> Maybe (Either Unit User)
  -> Either String FullArticle
  -> Array ArticleStub
  -> Aff (Response ResponseBody)
renderArticle env user article mostReadArticles = do
  let mosaico = MosaicoServer.app
  case article of
    Right a -> do
      let articleJSX =
            Article.render
              { paper: mosaicoPaper
              , article: Right a
              , onLogin: pure unit
              , user: hush =<< user
              , onPaywallEvent: pure unit
              , onTagClick: const mempty
              , onArticleClick: const mempty
              , mostReadArticles
              }
          mosaicoString = DOM.renderToString
                          $ mosaico
                            { mainContent: ArticleContent articleJSX
                            , mostReadArticles
                            , categoryStructure: env.categoryStructure
                            , user: hush =<< user
                            }

      html <- liftEffect do
        let windowVars =
              [ "article"           /\ (encodeStringifyArticle $ fromFullArticle a)
              , "isPreview"         /\ (show $ isPreviewArticle a)
              , "mostReadArticles"  /\ (encodeStringifyArticleStubs mostReadArticles)
              , "isDraft"           /\ (show $ isDraftArticle a)
              , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
              ] <> userVar user
            metaTags =
              let a' = fromFullArticle a
              in DOM.renderToStaticMarkup $
                  DOM.fragment
                    [ DOM.meta { property: "og:type", content: "article" }
                    , DOM.meta { property: "og:title", content: a'.title }
                    , DOM.meta { property: "og:description", content: fold a'.preamble }
                    , DOM.meta { property: "og:image", content: foldMap _.url a'.mainImage }
                    ]
        appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars) >>= appendHead metaTags

      pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody html
    Left _ ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
      in notFound env (notFoundArticleContent $ hush =<< user) user maybeMostRead

assets :: { params :: { path :: List String } } -> Aff (Either Failure File)
assets { params: { path } } = Handlers.directory "dist/client" path

frontpage :: Env -> { guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
frontpage env { guards: { credentials } } = do
  { user, articles, mostReadArticles } <- sequential $
    { user: _, articles: _, mostReadArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (getFrontpage mosaicoPaper "Startsidan")
    <*> parallel (Lettera.getMostRead 0 10 Nothing mosaicoPaper true)
  let mosaico = MosaicoServer.app
      renderFrontpage :: ArticleFeed -> JSX
      renderFrontpage (ArticleList list) =
        Frontpage.render $ Frontpage.List
          { content: Just list
          , onArticleClick: const mempty
          , onTagClick: const mempty
          }
      renderFrontpage (Html html) =
        Frontpage.render $ Frontpage.Prerendered
          { content: Just html
          , hooks: [ Frontpage.MostRead mostReadArticles (const $ pure unit) ]
          }
      frontpage' = renderFrontpage articles
      mosaicoString =
        DOM.renderToString
        $ mosaico
          { mainContent:
              case articles of
                ArticleList _ -> FrontpageContent frontpage'
                Html _        -> HtmlFrontPageContent frontpage'
          , mostReadArticles
          , categoryStructure: env.categoryStructure
          , user: hush =<< user
          }
  html <- liftEffect do
            let windowVars =
                  [ "frontpageFeed"     /\ mkArticleFeed Nothing "categoryfeed" articles
                  , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> userVar user
            appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
  pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody html

getFrontpage :: Paper -> String -> Aff ArticleFeed
getFrontpage paper category = do
  eitherHtml <- Lettera.getFrontpageHtml paper category
  case eitherHtml of
    Right html -> pure $ Html html
    Left err -> case err.type of
      Lettera.FrontPageHtmlNotFound -> getFrontpageArticles
      _                             -> do
        liftEffect $ Lettera.handleLetteraError err
        getFrontpageArticles
  where
    getFrontpageArticles = ArticleList <$> Lettera.getFrontpage paper Nothing

mkArticleFeed :: Maybe String -> String -> ArticleFeed -> String
mkArticleFeed feedPage feedType feed =
  let feedContent /\ feedContentType =
        case feed of
          ArticleList list -> encodeStringifyArticleStubs list /\ "articlelist"
          Html html        -> html /\ "html"
   in
    stringify $ encodeJson { feedPage, feedType, feedContent, feedContentType }

menu :: Env -> {} -> Aff (Response ResponseBody)
menu env _ = do
  let mosaico = MosaicoServer.app
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
            MenuContent
            $ Menu.render
                { categoryStructure: env.categoryStructure
                , onCategoryClick: const $ handler_ $ pure unit
                , user: Nothing
                , onLogout: pure unit
                , router: emptyRouter
                }
            , mostReadArticles: mempty
            , categoryStructure: env.categoryStructure
            , user: Nothing
          }
  html <- liftEffect do
            let windowVars =
                  [ "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ]
            appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
  pure $ htmlContent $ Response.ok $ StringBody html

tagList :: Env -> { params :: { tag :: String }, guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
tagList env { params: { tag }, guards: { credentials } } = do
  let tag' = uriComponentToTag tag
      mosaico = MosaicoServer.app
  { user, articles, mostReadArticles } <- sequential $
    { user: _, articles: _, mostReadArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (Lettera.getByTag 0 20 tag' mosaicoPaper)
    <*> parallel (Lettera.getMostRead 0 10 Nothing mosaicoPaper true)
  if null articles
    then notFound env (TagListContent tag' notFoundWithAside) user (Just mostReadArticles)
    else do
    let mosaicoString =
          DOM.renderToString
          $ mosaico
            { mainContent:
                TagListContent tag'
                $ Frontpage.render $ Frontpage.List
                  { content: Just articles
                  , onArticleClick: const mempty
                  , onTagClick: const mempty
                  }
            , categoryStructure: env.categoryStructure
            , mostReadArticles
            , user: hush =<< user
            }
    html <- liftEffect do
              let windowVars =
                    [ "frontpageFeed"     /\ mkArticleFeed (Just $ unwrap tag') "tagfeed" (ArticleList articles)
                    , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                    , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                    ] <> userVar user
              appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
    pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody html

staticPage :: Env -> { params :: { pageName :: String }, guards :: { credentials :: Maybe UserAuth }} -> Aff (Response ResponseBody)
staticPage env { params: { pageName }, guards: { credentials } } = do
  { user, mostReadArticles } <- sequential $
    { user: _, mostReadArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (Lettera.getMostRead 0 10 Nothing mosaicoPaper true)
  case HashMap.lookup (pageName <> ".html") env.staticPages of
    Just staticPageContent -> do
      let staticPageScript = HashMap.lookup (pageName <> ".js") env.staticPages
          mosaico = MosaicoServer.app
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
              { mainContent: StaticPageContent pageName staticPageJsx
              , mostReadArticles
              , categoryStructure: env.categoryStructure
              , user: hush =<< user
              }
      html <- liftEffect do
        let windowVars =
              [ "staticPageName" /\ (JSON.stringify $ JSON.fromString pageName)
              , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
              ] <> userVar user
        appendMosaico mosaicoString env.htmlTemplate
          >>= appendHead (mkWindowVariables windowVars)

      pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody html
    Nothing ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
      in notFound env (StaticPageContent pageName notFoundWithAside) user maybeMostRead

debugList :: Env -> { params :: { uuid :: String }, guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
debugList env { params: { uuid }, guards: { credentials } } = do
  { user, article, mostReadArticles } <- sequential $
    { user: _, article: _, mostReadArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> maybe (pure Nothing) (parallel <<< map hush <<< Lettera.getArticleStub) (UUID.parseUUID uuid)
    <*> parallel (Lettera.getMostRead 0 10 Nothing mosaicoPaper true)
  let mosaico = MosaicoServer.app
      mosaicoString =
        DOM.renderToString
        $ mosaico
          { mainContent:
              FrontpageContent $ Frontpage.render $ Frontpage.List
                { content: pure <$> article
                , onArticleClick: const mempty
                , onTagClick: const mempty
                }
          , mostReadArticles
          , categoryStructure: env.categoryStructure
          , user: hush =<< user
          }
  html <- liftEffect do
            let windowVars =
                  [ "frontpageFeed"     /\ mkArticleFeed Nothing "categoryfeed" (ArticleList $ fromFoldable article)
                  , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> userVar user
            appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
  pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody html

categoryPage :: Env -> { params :: { categoryName :: String }, guards :: { category :: Category, credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
categoryPage env { params: { categoryName }, guards: { credentials } } = do
  { user, articles, mostReadArticles } <- sequential $
    { user: _, articles: _, mostReadArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> parallel (Lettera.getFrontpage mosaicoPaper (Just categoryName))
    <*> parallel (Lettera.getMostRead 0 10 Nothing mosaicoPaper true)
  let mosaico = MosaicoServer.app
      mosaicoString = DOM.renderToString
                          $ mosaico
                            { mainContent: FrontpageContent $ Frontpage.render $ Frontpage.List
                                { content: Just articles
                                , onArticleClick: const mempty
                                , onTagClick: const mempty
                                }
                            , mostReadArticles
                            , categoryStructure: env.categoryStructure
                            , user: hush =<< user
                            }
  html <- liftEffect do
            let windowVars =
                  [ "frontpageFeed"     /\ mkArticleFeed (Just categoryName) "categoryfeed" (ArticleList articles)
                  , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> userVar user
            appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
  pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody html

searchPage :: Env -> { query :: { search :: Maybe String }, guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
searchPage env { query: { search }, guards: { credentials } } = do
  let query = if (trim <$> search) == Just "" then Nothing else search
  searchComponent <- liftEffect Search.searchComponent
  { user, articles, mostReadArticles } <- sequential $
    { user: _, articles: _, mostReadArticles: _ }
    <$> maybe (pure Nothing) (parallel <<< getUser) credentials
    <*> maybe (pure mempty) (parallel <<< Lettera.search 0 20 mosaicoPaper) query
    <*> parallel (Lettera.getMostRead 0 10 Nothing mosaicoPaper true)
  let mosaico = MosaicoServer.app
      mosaicoString = DOM.renderToString
                        $ mosaico
                          { mainContent: FrontpageContent $
                             searchComponent { query
                                             , doSearch: const $ pure unit
                                             , searching: false
                                             , noResults: isJust query && null articles
                                             } <>
                             (guard (not $ null articles) $
                              Frontpage.render $ Frontpage.List
                                { content: Just articles
                                , onArticleClick: const mempty
                                , onTagClick: const mempty
                                }
                             )
                          , mostReadArticles
                          , categoryStructure: env.categoryStructure
                          , user: hush =<< user
                          }
  html <- liftEffect do
            let windowVars =
                  [ "frontpageFeed"     /\ (mkArticleFeed query "searchfeed" $ ArticleList articles)
                  , "mostReadArticles"  /\ encodeStringifyArticleStubs mostReadArticles
                  , "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
                  ] <> userVar user
            appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
  pure $ maybeInvalidateAuth user $ htmlContent $ Response.ok $ StringBody html

notFoundPage :: Env -> { params :: { path :: List String }, guards :: { credentials :: Maybe UserAuth } } -> Aff (Response ResponseBody)
notFoundPage env { guards: { credentials } } = do
  user <- maybe (pure Nothing) getUser credentials
  notFound env (notFoundArticleContent $ hush =<< user) user mempty

notFoundArticleContent :: Maybe User -> MainContent
notFoundArticleContent user =
  ArticleContent $ Article.render
    { paper: mosaicoPaper
    , article: Right notFoundArticle
    , onLogin: pure unit
    , user
    , onPaywallEvent: pure unit
    , onTagClick: const mempty
    , onArticleClick: const mempty
    , mostReadArticles: mempty
    }

notFound :: Env -> MainContent -> Maybe (Either Unit User) -> Maybe (Array ArticleStub) -> Aff (Response ResponseBody)
notFound env mainContent user maybeMostReadArticles = do
  let mosaico = MosaicoServer.app
      mosaicoString = DOM.renderToString $ mosaico
                        { mainContent
                        , mostReadArticles: fromMaybe [] maybeMostReadArticles
                        , categoryStructure: env.categoryStructure
                        , user: hush =<< user
                        }
  html <- liftEffect $ do
    let windowVars =
          [ "categoryStructure" /\ (JSON.stringify $ encodeJson env.categoryStructure)
          ] <> userVar user
          <> foldMap (pure <<< Tuple "mostReadArticles" <<< encodeStringifyArticleStubs) maybeMostReadArticles
          <> (case mainContent of
                 ArticleContent _ -> [ "article" /\ (encodeStringifyArticle $ fromFullArticle notFoundArticle) ]
                 TagListContent tag _ -> [ "frontpageFeed" /\ mkArticleFeed (Just $ unwrap tag) "tagfeed" (ArticleList []) ]
                 StaticPageContent pageName _ -> [ "staticPageName" /\ (JSON.stringify $ JSON.fromString pageName) ]
                 _ -> mempty
             )
    appendMosaico mosaicoString env.htmlTemplate >>= appendHead (mkWindowVariables windowVars)
  pure $ maybeInvalidateAuth user $ htmlContent $ Response.notFound $ StringBody $ html

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
