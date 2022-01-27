module Mosaico where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (mapMaybe, null)
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..), hush)
import Data.Foldable (fold, foldMap)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Time.Duration (Minutes(..))
import Data.Tuple (Tuple)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Foreign (unsafeFromForeign)
import KSF.Auth (enableCookieLogin) as Auth
import KSF.Paper as Paper
import KSF.User (User, magicLogin)
import KSF.User.Cusno (Cusno)
import Lettera as Lettera
import Lettera.Models (Article, ArticleStub, Categories, Category(..), CategoryLabel(..), CategoryType(..), FullArticle(..), categoriesMap, fromFullArticle, frontpageCategoryLabel, isPreviewArticle, notFoundArticle, parseArticleStubWithoutLocalizing, parseArticleWithoutLocalizing, tagToURIComponent)
import Mosaico.Analytics (pushToDataLayer)
import Mosaico.Article as Article
import Mosaico.Error as Error
import Mosaico.Eval (ScriptTag(..), evalExternalScripts)
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), FeedSnapshot, JSInitialFeed, parseFeed)
import Mosaico.Footer (footer)
import Mosaico.Frontpage (Frontpage(..), render) as Frontpage
import Mosaico.Frontpage.Events (onFrontpageClick)
import Mosaico.Frontpage.Models (Hook(..)) as Frontpage
import Mosaico.Header as Header
import Mosaico.Header.Menu as Menu
import Mosaico.LoginModal as LoginModal
import Mosaico.MostReadList as MostReadList
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Routes as Routes
import Mosaico.Search as Search
import Mosaico.StaticPage (StaticPageResponse(..), fetchStaticPage, getInitialStaticPageContent, getInitialStaticPageScript)
import Mosaico.Webview as Webview
import Persona as Persona
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component, Render, UseEffect, UseState, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (write)
import Web.HTML (window) as Web
import Web.HTML.Window (scroll) as Web

data ModalView = LoginModal

type State =
  { article :: Maybe FullArticle
  , mostReadArticles :: Array ArticleStub
  , route :: Routes.MosaicoPage
  , prevRoute :: Maybe Routes.MosaicoPage
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , user :: Maybe User
  , staticPage :: Maybe StaticPageResponse
  , categoryStructure :: Array Category
  , catMap :: Categories
  , frontpageFeeds :: HashMap ArticleFeedType FeedSnapshot
  }

type SetState = (State -> State) -> Effect Unit

type Components =
  { loginModalComponent :: LoginModal.Props -> JSX
  , searchComponent :: Search.Props -> JSX
  , webviewComponent :: Webview.Props -> JSX
  , articleComponent :: Article.Props -> JSX
  }

type Props =
  { article :: Maybe FullArticle
  , mostReadArticles :: Maybe (Array ArticleStub)
  , staticPageName :: Maybe String
  , categoryStructure :: Array Category
  , initialFrontpageFeed :: HashMap ArticleFeedType ArticleFeed
  , user :: Maybe User
  }
type JSProps =
  { article :: Nullable Json
  , isPreview :: Nullable Boolean
  , mostReadArticles :: Nullable (Array Json)
  , staticPageName :: Nullable String
  , categoryStructure :: Nullable (Array Json)
  , initialFrontpageFeed :: Nullable JSInitialFeed
  , user :: Nullable Json
  }

app :: Component Props
app = do
  Auth.enableCookieLogin
  initialValues <- getInitialValues
  component "Mosaico" $ mosaicoComponent initialValues

mosaicoComponent
  :: InitialValues
  -> Props
  -> Render Unit (UseEffect (Tuple Routes.MosaicoPage (Maybe Cusno)) (UseEffect Unit (UseState State Unit))) JSX
mosaicoComponent initialValues props = React.do
  let initialCatMap = categoriesMap props.categoryStructure
      maxAge = Minutes 15.0
  state /\ setState <- useState initialValues.state
                         { article = props.article
                         , mostReadArticles = fold props.mostReadArticles
                         , staticPage = map StaticPageResponse $
                                        { pageName:_, pageContent:_, pageScript: initialValues.staticPageScript }
                                        <$> props.staticPageName
                                        <*> initialValues.staticPageContent
                         , categoryStructure = props.categoryStructure
                         , catMap = initialCatMap
                         , frontpageFeeds = map ({stamp: initialValues.startTime, feed: _}) props.initialFrontpageFeed
                         , route = fromMaybe Routes.Frontpage $ hush $
                                   match (Routes.routes initialCatMap) initialValues.locationState.path
                         , user = props.user
                         }

  let loadArticle articleId = Aff.launchAff_ do
        case UUID.parseUUID articleId of
          Nothing -> liftEffect $ setState _ { article = Nothing }
          Just uuid -> do
            eitherArticle <- Lettera.getArticleAuth uuid mosaicoPaper
            liftEffect case eitherArticle of
            --analytiikka tähän
              Right article -> do
                Article.evalEmbeds $ fromFullArticle article
                sendArticleAnalytics (fromFullArticle article) state.user
                setState _ { article = Just article }
              Left _ -> setState _ { article = Nothing }

  useEffectOnce do
    foldMap (Article.evalEmbeds <<< fromFullArticle) props.article
    Aff.launchAff_ do
      cats <- if null props.categoryStructure
              then Lettera.getCategoryStructure mosaicoPaper
              else pure props.categoryStructure
      let catMap = categoriesMap cats
      liftEffect do
        setState _ { categoryStructure = cats
                   , catMap = catMap
                   }
        -- Listen for route changes and set state accordingly
        void $ locations (routeListener catMap setState) initialValues.nav
      when (isNothing props.user) $
        magicLogin Nothing $ \u -> case u of
          Right user -> setState _ { user = Just user }
          _ -> pure unit
    pure mempty

  let loadFeed feedName = do
        maybeFeed <- case feedName of
          TagFeed t -> Just <<< ArticleList <$> Lettera.getByTag 0 20 t mosaicoPaper
          CategoryFeed c
            | Just cat <- unwrap <$> Map.lookup c state.catMap ->
              let label = unwrap c
                  getArticleList = ArticleList <$> Lettera.getFrontpage mosaicoPaper (Just label)
              in case cat.type of
                Prerendered ->
                  map Just <<< maybe getArticleList pure =<<
                  map Html <<< hush <$> Lettera.getFrontpageHtml mosaicoPaper label
                Feed -> Just <$> getArticleList
                _ -> pure Nothing
          CategoryFeed _ -> pure Nothing
          SearchFeed q -> Just <<< ArticleList <$> Lettera.search 0 20 mosaicoPaper q
        stamp <- liftEffect Now.nowDateTime
        foldMap (\feed -> liftEffect $ setState \s -> s { frontpageFeeds = HashMap.insert feedName { stamp, feed } s.frontpageFeeds }) maybeFeed
      setFrontpage feedName =
        case HashMap.lookup feedName state.frontpageFeeds of
          Nothing -> Aff.launchAff_ $ loadFeed feedName
          Just { stamp } -> do
            now <- Now.nowDateTime
            when (DateTime.diff now stamp > maxAge) do
              setState \s -> s { frontpageFeeds = HashMap.delete feedName s.frontpageFeeds }
              Aff.launchAff_ $ loadFeed feedName
      onPaywallEvent = do
        maybe (pure unit) loadArticle $ _.uuid <<< fromFullArticle <$> state.article

  useEffect (state.route /\ map _.cusno state.user) do
    case state.route of
      Routes.Frontpage -> setFrontpage (CategoryFeed frontpageCategoryLabel)
      Routes.TagPage tag -> setFrontpage (TagFeed tag)
      Routes.SearchPage Nothing -> pure unit
      Routes.SearchPage (Just query) -> setFrontpage (SearchFeed query)
      -- Always uses server side provided article
      Routes.DraftPage -> pure unit
      Routes.ArticlePage articleId
        | (isPreviewArticle <$> state.article) == Just true -> loadArticle articleId
        | Just articleId == ((_.uuid <<< fromFullArticle) <$> state.article) -> pure unit
        | otherwise -> loadArticle articleId
      Routes.MenuPage -> pure unit
      Routes.NotFoundPage _path -> pure unit
      Routes.CategoryPage (Category c) -> setFrontpage (CategoryFeed c.label)
      Routes.StaticPage page
        | Just (StaticPageResponse r) <- state.staticPage
        , r.pageName == page
        -> when (isJust state.prevRoute) do
             foldMap (\p -> evalExternalScripts [ScriptTag $ "<script>" <> p <> "</script>"]) r.pageScript
        | otherwise ->
          Aff.launchAff_ do
            staticPage <- fetchStaticPage page
            liftEffect $ setState _  { staticPage = Just staticPage }
            case staticPage of
              StaticPageResponse r
                | Just p <- r.pageScript -> liftEffect $ evalExternalScripts [ScriptTag $ "<script>" <> p <> "</script>"]
              _ -> mempty
      Routes.DebugPage _ -> pure unit

    case props.mostReadArticles of
      Just mostReads
        | not $ null mostReads -> liftEffect $ setState \s -> s { mostReadArticles = mostReads }
      _ ->
        Aff.launchAff_ do
          mostReadArticles <- Lettera.getMostRead 0 10 Nothing mosaicoPaper true
          liftEffect $ setState \s -> s { mostReadArticles = mostReadArticles }

    pure mempty

  pure $ render setState state initialValues.components initialValues.nav onPaywallEvent

sendArticleAnalytics:: Article -> Maybe User -> Effect Unit
sendArticleAnalytics article user = do
  let metadata = { title: article.title
          , publishingTime: article.publishingTimeUtc
          , authors: article.authors
          , premium: article.premium
          , category: article.analyticsCategory
          , section: article.analyticsSection
          , listTitle: article.listTitle
          , articleUuid: article.uuid
          , tags: article.tags
          , userCusno: case user of
            Just u -> Just u.cusno
            Nothing   -> Nothing
          , userSubs: case user of
            Just u -> Just u.subs
            Nothing -> Nothing
          }
  pushToDataLayer metadata
  pure unit

routeListener :: Categories -> ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener c setState _oldLoc location = do
  case match (Routes.routes c) location.path of
    Right path -> setState \s -> s { route = path, prevRoute = Just s.route }
    Left _     -> pure unit

type InitialValues =
  { state :: State
  , components :: Components
  , nav :: PushStateInterface
  , locationState :: LocationState
  , staticPageContent :: Maybe String
  , staticPageScript :: Maybe String
  , startTime :: DateTime
  }

getInitialValues :: Effect InitialValues
getInitialValues = do
  startTime <- Now.nowDateTime
  nav <- makeInterface
  locationState <- nav.locationState
  staticPageContent <- toMaybe <$> getInitialStaticPageContent
  staticPageScript <- toMaybe <$> getInitialStaticPageScript

  loginModalComponent <- LoginModal.loginModal
  searchComponent     <- Search.searchComponent
  webviewComponent    <- Webview.webviewComponent
  articleComponent    <- Article.component
  pure
    { state:
        { article: Nothing
        , mostReadArticles: []
        , route: Routes.Frontpage
        , prevRoute: Nothing
        , clickedArticle: Nothing
        , modalView: Nothing
        , user: Nothing
        , staticPage: Nothing
        , categoryStructure: []
        , catMap: Map.empty
        , frontpageFeeds: HashMap.empty
        }
    , components:
        { loginModalComponent
        , searchComponent
        , webviewComponent
        , articleComponent
        }
    , nav
    , locationState
    , staticPageContent
    , staticPageScript
    , startTime
    }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  let isPreview = fromMaybe false $ toMaybe jsProps.isPreview
      mkFullArticle
        | isPreview = PreviewArticle
        | otherwise = FullArticle
      article = mkFullArticle <$> (hush <<< parseArticleWithoutLocalizing =<< toMaybe jsProps.article)
      mostReadArticles = map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ toMaybe jsProps.mostReadArticles

      initialFrontpageFeed = maybe HashMap.empty parseFeed $ toMaybe jsProps.initialFrontpageFeed

      staticPageName = toMaybe jsProps.staticPageName
      -- Decoding errors are being hushed here, although if this
      -- comes from `window.categoryStructure`, they should be
      -- valid categories
      categoryStructure = foldMap (mapMaybe (hush <<< decodeJson)) $ toMaybe jsProps.categoryStructure
      -- User comes directly from the server, which uses the same
      -- version of User.  User is alreay quite close to native
      -- JavaScript representation, which should make raw conversion
      -- to and from possible.
      user = unsafeFromForeign <<< Persona.rawJSONParse <<< stringify <$> toMaybe jsProps.user
  in { article, mostReadArticles, initialFrontpageFeed, staticPageName, categoryStructure, user }

jsApp :: Effect (React.ReactComponent JSProps)
jsApp = do
  Auth.enableCookieLogin
  initialValues <- getInitialValues
  React.reactComponent "Mosaico" $ mosaicoComponent initialValues <<< fromJSProps

render :: SetState -> State -> Components -> PushStateInterface -> Effect Unit -> JSX
render setState state components router onPaywallEvent =
  case state.modalView of
    Just LoginModal ->
      components.loginModalComponent
        { onUserFetch: \user ->
           case user of
             Right u -> do
               setState _ { modalView = Nothing, user = Just u }
               onPaywallEvent
             Left _err -> do
               onPaywallEvent
               -- TODO: Handle properly
               Console.error $ "Login error " <> show _err
        , onClose: setState \s -> s { modalView = Nothing }
        }
    _ -> mempty
  <> case state.route of
       Routes.CategoryPage category@(Category c)
         | c.type == Webview -> mosaicoLayoutNoAside $ components.webviewComponent { category }
         | otherwise ->
           let maybeFeed = _.feed <$> HashMap.lookup (CategoryFeed c.label) state.frontpageFeeds
            in frontpageNoHeader maybeFeed
       Routes.ArticlePage articleId
         | Just fullArticle <- state.article
         , article <- fromFullArticle fullArticle
         -- If we have this article already in `state`, let's pass that to `articleComponent`
         , article.uuid == articleId -> mosaicoLayoutNoAside $ renderArticle (Right fullArticle)
         | Just stub <- state.clickedArticle -> mosaicoLayoutNoAside $ renderArticle $ Left stub
         | otherwise -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.Frontpage -> frontpageNoHeader $ _.feed <$> HashMap.lookup (CategoryFeed frontpageCategoryLabel) state.frontpageFeeds
       Routes.SearchPage Nothing ->
          mosaicoDefaultLayout $ components.searchComponent { query: Nothing, doSearch, searching: false, noResults: false }
       Routes.SearchPage query@(Just queryString) ->
          let frontpageArticles = _.feed <$> HashMap.lookup (SearchFeed queryString) state.frontpageFeeds
              searching = isNothing frontpageArticles
              noResults = case frontpageArticles of
                Just (ArticleList list)
                  | null list -> true
                _             -> false
              searchProps = { query, doSearch, searching, noResults }
              header = components.searchComponent searchProps
          in frontpageWithHeader header frontpageArticles
       Routes.NotFoundPage _ -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.TagPage tag ->
         let maybeFeed = _.feed <$> HashMap.lookup (TagFeed tag) state.frontpageFeeds
          in case maybeFeed of
               Just (ArticleList tagFeed)
                 | null tagFeed -> mosaicoDefaultLayout Error.notFoundWithAside
               _                -> frontpageNoHeader maybeFeed
       Routes.MenuPage ->
         mosaicoLayoutNoAside
         $ Menu.render
             { router
             , categoryStructure: state.categoryStructure
             , onCategoryClick
             , user: state.user
             , onLogout: do
                 setState _ { user = Nothing }
                 onPaywallEvent
             }
       Routes.DraftPage -> mosaicoLayoutNoAside
         $ renderArticle $ maybe (Right notFoundArticle) Right state.article
       Routes.StaticPage _ -> mosaicoDefaultLayout $ case state.staticPage of
         Nothing -> DOM.text "laddar"
         Just (StaticPageResponse page)  ->
           DOM.div { className: "mosaico--static-page", dangerouslySetInnerHTML: { __html: page.pageContent } }
         Just StaticPageNotFound -> Error.notFoundWithAside
         Just StaticPageOtherError -> Error.somethingWentWrong
       Routes.DebugPage _ -> frontpageNoHeader $ _.feed <$> HashMap.lookup (CategoryFeed $ CategoryLabel "") state.frontpageFeeds
  where

    frontpageWithHeader :: JSX -> Maybe ArticleFeed -> JSX
    frontpageWithHeader header = frontpage $ Just header

    frontpageNoHeader :: Maybe ArticleFeed -> JSX
    frontpageNoHeader = frontpage Nothing

    frontpage :: Maybe JSX -> Maybe ArticleFeed -> JSX
    frontpage maybeHeader (Just (ArticleList list)) = listFrontpage maybeHeader $ Just list
    frontpage maybeHeader (Just (Html html))        = prerenderedFrontpage maybeHeader $ Just html
    frontpage maybeHeader _                         = listFrontpage maybeHeader Nothing

    listFrontpage :: Maybe JSX -> Maybe (Array ArticleStub) -> JSX
    listFrontpage maybeHeader content = mosaicoDefaultLayout $
      (fromMaybe mempty maybeHeader) <>
      (Frontpage.render $ Frontpage.List
        { content
        , onArticleClick
        , onTagClick
        })

    prerenderedFrontpage :: Maybe JSX -> Maybe String -> JSX
    prerenderedFrontpage maybeHeader content =
      mosaicoLayout inner false (onFrontpageClick simpleRoute)
      where
        inner =
          (fromMaybe mempty maybeHeader) <>
          (Frontpage.render $ Frontpage.Prerendered
             { content
             , hooks
             })

    hooks :: Array Frontpage.Hook
    hooks = [ Frontpage.MostRead state.mostReadArticles onClickHandler
            , Frontpage.ArticleUrltoRelative
            ]

    mosaicoDefaultLayout :: JSX -> JSX
    mosaicoDefaultLayout content = mosaicoLayout content true mempty

    mosaicoLayoutNoAside :: JSX -> JSX
    mosaicoLayoutNoAside content = mosaicoLayout content false mempty

    mosaicoLayout :: JSX -> Boolean -> EventHandler -> JSX
    mosaicoLayout content showAside onClick = DOM.div
      { className: "mosaico grid"
      , id: Paper.toString mosaicoPaper
      , onClick
      , children:
          [ Header.topLine
          , Header.render
              { router
              , categoryStructure: state.categoryStructure
              , catMap: state.catMap
              , onCategoryClick
              , user: state.user
              , onLogin
              }
          , Header.mainSeparator
          , content
          , footer onStaticPageClick
          , guard showAside $ DOM.aside
              { className: "mosaico--aside"
              , children:
                  [ MostReadList.render
                      { mostReadArticles: state.mostReadArticles
                      , onClickHandler
                      }
                  ]
              }
          ]
      }

    renderArticle :: Either ArticleStub FullArticle -> JSX
    renderArticle article =
      components.articleComponent
        { paper: mosaicoPaper
        , article
        , onLogin
        , user: state.user
        , onPaywallEvent
        , onTagClick
        , onArticleClick
        , mostReadArticles: state.mostReadArticles
        }

    onClickHandler articleStub = do
      setState _ { clickedArticle = Just articleStub }
      void $ Web.scroll 0 0 =<< Web.window
      simpleRoute $ "/artikel/" <> articleStub.uuid

    onCategoryClick cat@(Category c) =
      case state.route of
        Routes.CategoryPage category | category == cat -> mempty
        _ -> capture_ do
          void $ Web.scroll 0 0 =<< Web.window
          simpleRoute $ "/" <> if c.label == frontpageCategoryLabel then "" else show c.label

    onTagClick tag = capture_ do
      void $ Web.scroll 0 0 =<< Web.window
      simpleRoute $ "/tagg/" <> tagToURIComponent tag

    onArticleClick article = capture_ $ handleArticleClick article

    handleArticleClick article = do
      setState _ { clickedArticle = Just article }
      void $ Web.scroll 0 0 =<< Web.window
      simpleRoute $ "/artikel/" <> article.uuid

    onStaticPageClick link =
      case state.route of
        Routes.StaticPage page | page == link -> mempty
        _ -> capture_ do
          void $ Web.scroll 0 0 =<< Web.window
          router.pushState (write {}) ("/sida/" <> link)

    onLogin = setState \s -> s { modalView = Just LoginModal }

    -- Search is done via the router
    doSearch query = do
      router.pushState (write {}) $ "/sök?q=" <> query

    simpleRoute = router.pushState (write {})