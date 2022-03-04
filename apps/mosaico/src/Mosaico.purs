module Mosaico where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (fromFoldable, mapMaybe, null)
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
import Data.Set (Set)
import Data.Time.Duration (Minutes(..))
import Data.Tuple (Tuple)
import Data.UUID as UUID
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff as Aff
import Effect.Aff.AVar as Aff.AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Foreign (unsafeFromForeign)
import KSF.Auth (enableCookieLogin) as Auth
import KSF.Paper as Paper
import KSF.Spinner (loadingSpinner)
import KSF.User (User, logout, magicLogin)
import KSF.User.Cusno (Cusno)
import Lettera as Lettera
import Lettera.Models (ArticleStub, Categories, Category(..), CategoryLabel(..), CategoryType(..), FullArticle, categoriesMap, frontpageCategoryLabel, notFoundArticle, parseArticleStubWithoutLocalizing, parseArticleWithoutLocalizing, readArticleType, tagToURIComponent)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.Analytics (sendArticleAnalytics)
import Mosaico.Article as Article
import Mosaico.Epaper as Epaper
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
import Mosaico.LatestList as LatestList
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Profile as Profile
import Mosaico.Routes as Routes
import Mosaico.Search as Search
import Mosaico.StaticPage (StaticPageResponse(..), fetchStaticPage, getInitialStaticPageContent, getInitialStaticPageScript)
import Mosaico.Webview as Webview
import Persona as Persona
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, Render, UseEffect, UseState, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (write)
import Web.HTML (window) as Web
import Web.HTML.Window (scroll) as Web

data ModalView = LoginModal

type State =
  { article :: Maybe (Either Unit FullArticle)
  , mostReadArticles :: Array ArticleStub
  , latestArticles :: Array ArticleStub
  , route :: Routes.MosaicoPage
  , prevRoute :: Maybe Routes.MosaicoPage
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , user :: Maybe User
  , entitlements :: Maybe (Set String)
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
  , epaperComponent :: Epaper.Props -> JSX
  }

type Props =
  { article :: Maybe FullArticle
  , mostReadArticles :: Maybe (Array ArticleStub)
  , latestArticles :: Maybe (Array ArticleStub)
  , staticPageName :: Maybe String
  , categoryStructure :: Array Category
  , initialFrontpageFeed :: HashMap ArticleFeedType ArticleFeed
  , user :: Maybe User
  , entitlements :: Maybe (Set String)
  }
type JSProps =
  { article :: Nullable Json
  , articleType :: Nullable String
  , mostReadArticles :: Nullable (Array Json)
  , latestArticles :: Nullable (Array Json)
  , staticPageName :: Nullable String
  , categoryStructure :: Nullable (Array Json)
  , initialFrontpageFeed :: Nullable JSInitialFeed
  , user :: Nullable Json
  , entitlements :: Nullable Json
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
  let initialPath = initialValues.locationState.path <> initialValues.locationState.search
      maxAge = Minutes 15.0
  state /\ setState <- useState initialValues.state
                         { article = Right <$> props.article
                         , mostReadArticles = fold props.mostReadArticles
                         , latestArticles = fold props.latestArticles
                         , staticPage = map StaticPageResponse $
                                        { pageName:_, pageContent:_, pageScript: initialValues.staticPageScript }
                                        <$> props.staticPageName
                                        <*> initialValues.staticPageContent
                         , categoryStructure = props.categoryStructure
                         , catMap = initialCatMap
                         , frontpageFeeds = map ({stamp: initialValues.startTime, feed: _}) props.initialFrontpageFeed
                         , route = fromMaybe Routes.Frontpage $ hush $
                                   match (Routes.routes initialCatMap) initialPath
                         , user = props.user
                         , entitlements = props.entitlements
                         }

  let loadArticle articleId = Aff.launchAff_ do
        case UUID.parseUUID articleId of
          Nothing -> liftEffect $ setState _ { article = Just $ Left unit }
          Just uuid -> do
            liftEffect $ setState _ { article = Nothing }
            eitherArticle <- Lettera.getArticleAuth uuid mosaicoPaper
            liftEffect case eitherArticle of
              Right article -> do
                Article.evalEmbeds article.article
                sendArticleAnalytics article.article state.user
                setState _ { article = Just $ Right article }
              Left _ -> setState _ { article = Just $ Left unit }

  useEffectOnce do
    foldMap (Article.evalEmbeds <<< _.article) props.article
    Aff.launchAff_ do
      when (not $ Map.isEmpty initialCatMap) $ Aff.AVar.put initialCatMap initialValues.catMap
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
      when (Map.isEmpty initialCatMap) $ Aff.AVar.put catMap initialValues.catMap
      when (isNothing props.user) $
        magicLogin Nothing $ \u -> case u of
          Right user -> setState _ { user = Just user }
          _ -> pure unit
    pure mempty

  let loadFeed feedName = do
        -- In SPA mode, this may be called before catMap has been
        -- populated to state.  Synchronize with an AVar.
        catMap <- Aff.AVar.read initialValues.catMap
        maybeFeed <- case feedName of
          TagFeed t -> Just <<< ArticleList <<< join <<< fromFoldable <$> Lettera.getByTag 0 20 t mosaicoPaper
          CategoryFeed c
            | Just cat <- unwrap <$> Map.lookup c catMap ->
              let label = unwrap c
                  getArticleList = ArticleList <<< join <<< fromFoldable <$> Lettera.getFrontpage mosaicoPaper (Just label) Nothing
              in case cat.type of
                Prerendered ->
                  map Just <<< maybe getArticleList pure =<<
                  map Html <<< Lettera.responseBody <$> Lettera.getFrontpageHtml mosaicoPaper label Nothing
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
        maybe (pure unit) loadArticle $ _.article.uuid <$> (join <<< map hush $ state.article)

  useEffect (state.route /\ map _.cusno state.user) do
    case state.route of
      Routes.Frontpage -> setFrontpage (CategoryFeed frontpageCategoryLabel)
      Routes.TagPage tag -> setFrontpage (TagFeed tag)
      Routes.SearchPage Nothing -> pure unit
      Routes.SearchPage (Just query) -> setFrontpage (SearchFeed query)
      -- Always uses server side provided article
      Routes.DraftPage -> pure unit
      Routes.ProfilePage -> pure unit
      Routes.ArticlePage articleId
        | Just articleId == (_.article.uuid <$> (join <<< map hush $ state.article)) -> pure unit
        | otherwise -> loadArticle articleId
      Routes.MenuPage -> pure unit
      Routes.NotFoundPage _path -> pure unit
      Routes.CategoryPage (Category c) -> setFrontpage (CategoryFeed c.label)
      Routes.EpaperPage -> pure unit
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
        | not $ null mostReads -> liftEffect $ setState _ { mostReadArticles = mostReads }
      _ ->
        Aff.launchAff_ do
          mostReadArticles <- join <<< fromFoldable <$> Lettera.getMostRead 0 10 Nothing mosaicoPaper true
          liftEffect $ setState \s -> s { mostReadArticles = mostReadArticles }

    case props.latestArticles of
      Just latest
        | not $ null latest -> liftEffect $ setState _ { latestArticles = latest }
      _ ->
        Aff.launchAff_ do
          latestArticles <- join <<< fromFoldable <$> Lettera.getLatest 0 10 mosaicoPaper
          liftEffect $ setState \s -> s { latestArticles = latestArticles }

    pure mempty

  pure $ render setState state initialValues.components initialValues.nav onPaywallEvent

routeListener :: Categories -> ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener c setState _oldLoc location = do
  case match (Routes.routes c) $ location.pathname <> location.search of
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
  , catMap :: AVar.AVar Categories
  }

getInitialValues :: Effect InitialValues
getInitialValues = do
  catMap <- AVar.empty
  startTime <- Now.nowDateTime
  nav <- makeInterface
  locationState <- nav.locationState
  staticPageContent <- toMaybe <$> getInitialStaticPageContent
  staticPageScript <- toMaybe <$> getInitialStaticPageScript

  loginModalComponent <- LoginModal.loginModal
  searchComponent     <- Search.searchComponent
  webviewComponent    <- Webview.webviewComponent
  articleComponent    <- Article.component
  epaperComponent     <- Epaper.component
  pure
    { state:
        { article: Nothing
        , mostReadArticles: []
        , latestArticles: []
        , route: Routes.Frontpage
        , prevRoute: Nothing
        , clickedArticle: Nothing
        , modalView: Nothing
        , user: Nothing
        , entitlements: Nothing
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
        , epaperComponent
        }
    , catMap
    , nav
    , locationState
    , staticPageContent
    , staticPageScript
    , startTime
    }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  let article = { articleType: _, article: _ }
                <$> (readArticleType =<< toMaybe jsProps.articleType)
                <*> (hush <<< parseArticleWithoutLocalizing =<< toMaybe jsProps.article)
      mostReadArticles = map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ toMaybe jsProps.mostReadArticles
      latestArticles = map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ toMaybe jsProps.latestArticles

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
      entitlements = foldMap (hush <<< decodeJson) $ toMaybe jsProps.entitlements
  in { article, mostReadArticles, latestArticles, initialFrontpageFeed, staticPageName, categoryStructure, user, entitlements }

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
       Routes.CategoryPage category -> renderCategory category
       Routes.ArticlePage articleId
         | Just (Right fullArticle@{ article }) <- state.article -> mosaicoLayoutNoAside $
           if article.uuid == articleId
           -- If we have this article already in `state`, let's pass that to `renderArticle`
           then renderArticle (Right fullArticle)
           else loadingSpinner
         | Just stub <- state.clickedArticle -> mosaicoLayoutNoAside $ renderArticle $ Left stub
         | Nothing <- state.article -> mosaicoLayoutNoAside loadingSpinner
         | otherwise -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.Frontpage -> maybe mempty renderCategory $ Map.lookup frontpageCategoryLabel state.catMap
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
               _                -> frontpageNoHeader Nothing maybeFeed
       Routes.MenuPage ->
         mosaicoLayoutNoAside
         $ Menu.render
             { router
             , categoryStructure: state.categoryStructure
             , onCategoryClick
             , user: state.user
             , onLogin
             , onLogout
             }
       Routes.EpaperPage -> mosaicoLayoutNoAside
         $ components.epaperComponent
             { user: state.user
             , entitlements: state.entitlements
             , paper: mosaicoPaper
             , onLogin
             }
       Routes.ProfilePage -> mosaicoLayoutNoAside
         $ Profile.render
             { user: state.user
             , onLogin
             , onLogout
             , onStaticPageClick
             }
       Routes.DraftPage -> mosaicoLayoutNoAside
         $ renderArticle $ maybe (Right notFoundArticle) Right $ join <<< map hush $ state.article
       Routes.StaticPage _ -> mosaicoDefaultLayout $ case state.staticPage of
         Nothing -> DOM.text "laddar"
         Just (StaticPageResponse page)  ->
           DOM.div { className: "mosaico--static-page", dangerouslySetInnerHTML: { __html: page.pageContent } }
         Just StaticPageNotFound -> Error.notFoundWithAside
         Just StaticPageOtherError -> Error.somethingWentWrong
       Routes.DebugPage _ -> frontpageNoHeader Nothing $ _.feed <$> HashMap.lookup (CategoryFeed $ CategoryLabel "debug") state.frontpageFeeds
  where

    renderCategory :: Category -> JSX
    renderCategory category@(Category c) =
      let maybeFeed = _.feed <$> HashMap.lookup (CategoryFeed c.label) state.frontpageFeeds
      in case c.type of
        Webview -> mosaicoLayoutNoAside $ components.webviewComponent { category }
        Link -> mempty -- TODO
        Prerendered -> maybe (mosaicoLayoutNoAside loadingSpinner) (frontpageNoHeader Nothing <<< Just) maybeFeed
        Feed -> frontpageNoHeader (Just c.label) maybeFeed

    frontpageWithHeader :: JSX -> Maybe ArticleFeed -> JSX
    frontpageWithHeader header = frontpage (Just header) Nothing

    frontpageNoHeader :: Maybe CategoryLabel -> Maybe ArticleFeed -> JSX
    frontpageNoHeader = frontpage Nothing

    frontpage :: Maybe JSX -> Maybe CategoryLabel -> Maybe ArticleFeed -> JSX
    frontpage maybeHeader maybeCategorLabel (Just (ArticleList list)) = listFrontpage maybeHeader maybeCategorLabel $ Just list
    frontpage maybeHeader _ (Just (Html html))                        = prerenderedFrontpage maybeHeader $ Just html
    frontpage maybeHeader _ _                                         = listFrontpage maybeHeader Nothing Nothing

    listFrontpage :: Maybe JSX -> Maybe CategoryLabel -> Maybe (Array ArticleStub) -> JSX
    listFrontpage maybeHeader maybeCategoryLabel content = mosaicoDefaultLayout $
      (fromMaybe mempty maybeHeader) <>
      (Frontpage.render $ Frontpage.List
        { categoryLabel: unwrap <$> maybeCategoryLabel
        , content
        , onArticleClick
        , onTagClick
        })

    prerenderedFrontpage :: Maybe JSX -> Maybe String -> JSX
    prerenderedFrontpage maybeHeader content =
      mosaicoLayout inner false
      where
        inner =
          (fromMaybe mempty maybeHeader) <>
          (Frontpage.render $ Frontpage.Prerendered
             { content
             , hooks
             , onClick: onFrontpageClick $
                \path -> setState _ { clickedArticle = Nothing } *> simpleRoute path
             })

    hooks :: Array Frontpage.Hook
    hooks = [ Frontpage.MostRead state.mostReadArticles onClickHandler
            , Frontpage.Latest state.latestArticles onClickHandler
            , Frontpage.ArticleUrltoRelative
            , Frontpage.Ad "Box Ad 1 DESKTOP" "mosaico-ad__firstbox"
            , Frontpage.Ad "Box Ad 2 DESKTOP" "mosaico-ad__sidebar-2"
            , Frontpage.Ad "Box Ad 3 DESKTOP" "mosaico-ad__sidebar-3"
            , Frontpage.Ad "Box Ad 4 DESKTOP" "mosaico-ad__sidebar-4"
            , Frontpage.Ad "Ad 1"             "mosaico-ad__bigbox1"
            , Frontpage.Ad "Ad 2"             "mosaico-ad__bigbox2"
            ]

    mosaicoDefaultLayout :: JSX -> JSX
    mosaicoDefaultLayout content = mosaicoLayout content true

    mosaicoLayoutNoAside :: JSX -> JSX
    mosaicoLayoutNoAside content = mosaicoLayout content false

    mosaicoLayout :: JSX -> Boolean -> JSX
    mosaicoLayout content showAside = DOM.div_
      [ Mosaico.ad { contentUnit: "mosaico-ad__top-parade" }
      , DOM.div
          { className: "mosaico grid"
          , id: Paper.toString mosaicoPaper
          , children:
              [ Header.topLine
              , Header.render
                  { router
                  , categoryStructure: state.categoryStructure
                  , catMap: state.catMap
                  , onCategoryClick
                  , user: state.user
                  , onLogin
                  , onProfile
                  , onStaticPageClick
                  }
              , Header.mainSeparator
              , Mosaico.ad { contentUnit: "mosaico-ad__parade" }
              , content
              , footer onStaticPageClick
              , guard showAside $ DOM.aside
                  { className: "mosaico--aside"
                  , children:
                      [ MostReadList.render
                          { mostReadArticles: state.mostReadArticles
                          , onClickHandler
                          }
                      , LatestList.render
                          { latestArticles: state.latestArticles
                          , onClickHandler
                          }
                      ]
                  }
              ]
          }
      ]

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
        , latestArticles: state.latestArticles
        }

    onClickHandler articleStub = capture_ do
      setState _ { clickedArticle = Just articleStub }
      simpleRoute $ "/artikel/" <> articleStub.uuid

    onCategoryClick cat@(Category c) =
      case state.route of
        Routes.CategoryPage category | category == cat -> mempty
        _ -> capture_ do
          simpleRoute $ "/" <> if c.label == frontpageCategoryLabel then "" else show c.label

    onProfile = capture_ $ simpleRoute "/konto"

    onTagClick tag = capture_ do
      simpleRoute $ "/tagg/" <> tagToURIComponent tag

    onArticleClick article = capture_ $ handleArticleClick article

    handleArticleClick article = do
      setState _ { clickedArticle = Just article }
      simpleRoute $ "/artikel/" <> article.uuid

    onStaticPageClick link =
      case state.route of
        Routes.StaticPage page | page == link -> mempty
        _ -> capture_ do
          void $ Web.scroll 0 0 =<< Web.window
          router.pushState (write {}) ("/sida/" <> link)

    onLogin = capture_ $ setState \s -> s { modalView = Just LoginModal }

    onLogout = capture_ do
      Aff.launchAff_ $ logout $ const $ pure unit
      setState _ { user = Nothing }
      onPaywallEvent

    -- Search is done via the router
    doSearch query = do
      router.pushState (write {}) $ "/sök?q=" <> query

    simpleRoute path = do
      void $ Web.scroll 0 0 =<< Web.window
      router.pushState (write {}) path
