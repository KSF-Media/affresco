module Mosaico where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (fromFoldable, index, length, mapMaybe, null)
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..), hush)
import Data.Foldable (fold, foldMap, elem)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Time.Duration (Minutes(..), Milliseconds(..))
import Data.Tuple (Tuple (..), snd)
import Data.UUID as UUID
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff as Aff
import Effect.Aff.AVar as Aff.AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Now as Now
import Effect.Random (randomInt)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import KSF.Auth (enableCookieLogin) as Auth
import KSF.Paper as Paper
import KSF.Sentry as Sentry
import KSF.Spinner (loadingSpinner)
import KSF.User (User, logout, magicLogin)
import KSF.User.Cusno (Cusno)
import Lettera as Lettera
import Lettera.Models (ArticleStub, ArticleType(..), Categories, Category(..), CategoryLabel(..), CategoryType(..), FullArticle, categoriesMap, frontpageCategoryLabel, notFoundArticle, parseArticleStubWithoutLocalizing, parseArticleWithoutLocalizing, readArticleType, tagToURIComponent)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.Analytics (sendArticleAnalytics, sendPageView)
import Mosaico.Article as Article
import Mosaico.Article.Advertorial.Basic as Advertorial.Basic
import Mosaico.Article.Advertorial.Standard as Advertorial.Standard
import Mosaico.Article.Image as Image
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
import Mosaico.LatestList as LatestList
import Mosaico.LoginModal as LoginModal
import Mosaico.MostReadList as MostReadList
import Mosaico.Paper (mosaicoPaper, _mosaicoPaper)
import Mosaico.Profile as Profile
import Mosaico.Routes as Routes
import Mosaico.Search as Search
import Mosaico.StaticPage (StaticPageResponse(..), fetchStaticPage, getInitialStaticPageContent, getInitialStaticPageScript)
import Mosaico.Webview as Webview
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, Render, UseEffect, UseState, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON as JSON
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (setTitle) as Web
import Web.HTML.Window (document, scroll) as Web

foreign import refreshAdsImpl :: EffectFn1 (Array String) Unit
foreign import sentryDsn_ :: Effect String

data ModalView = LoginModal

type State =
  { article :: Maybe (Either Unit FullArticle)
  , mostReadArticles :: Array ArticleStub
  , latestArticles :: Array ArticleStub
  , route :: Routes.MosaicoPage
  , prevRoute :: Maybe (Tuple Routes.MosaicoPage String)
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , user :: Maybe (Maybe User)
  , staticPage :: Maybe StaticPageResponse
  , categoryStructure :: Array Category
  , catMap :: Categories
  , frontpageFeeds :: HashMap ArticleFeedType FeedSnapshot
  , showAds :: Boolean
  , ssrPreview :: Boolean
  , advertorials :: Maybe (Array ArticleStub)
  , singleAdvertorial :: Maybe ArticleStub
  , logger :: Sentry.Logger
  }

type SetState = (State -> State) -> Effect Unit

type Components =
  { loginModalComponent :: LoginModal.Props -> JSX
  , searchComponent :: Search.Props -> JSX
  , webviewComponent :: Webview.Props -> JSX
  , articleComponent :: Article.Props -> JSX
  , epaperComponent :: Epaper.Props -> JSX
  , imageComponent :: Image.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  }

type Props =
  { article :: Maybe FullArticle
  , mostReadArticles :: Maybe (Array ArticleStub)
  , latestArticles :: Maybe (Array ArticleStub)
  , staticPageName :: Maybe String
  , categoryStructure :: Array Category
  , globalDisableAds :: Boolean
  , initialFrontpageFeed :: HashMap ArticleFeedType ArticleFeed
  }

type JSProps =
  { article :: Nullable Json
  , articleType :: Nullable String
  , mostReadArticles :: Nullable (Array Json)
  , latestArticles :: Nullable (Array Json)
  , staticPageName :: Nullable String
  , categoryStructure :: Nullable (Array Json)
  , globalDisableAds :: Nullable Boolean
  , initialFrontpageFeed :: Nullable JSInitialFeed
  }

app :: Component Props
app = do
  Auth.enableCookieLogin
  initialValues <- getInitialValues
  component "Mosaico" $ mosaicoComponent initialValues

getPathFromLocationState :: LocationState -> String
getPathFromLocationState locationState = Routes.stripFragment $ locationState.path <> locationState.search

mosaicoComponent
  :: InitialValues
  -> Props
  -> Render Unit (UseEffect (Tuple Routes.MosaicoPage (Maybe Cusno)) (UseEffect Unit (UseState State Unit))) JSX
mosaicoComponent initialValues props = React.do
  let setTitle t = Web.setTitle t =<< Web.document =<< Web.window
      initialCatMap = categoriesMap props.categoryStructure
      initialPath = getPathFromLocationState initialValues.locationState
      maxAge = Minutes 15.0
  state /\ setState_ <- useState initialValues.state
                         { article = Right <$> props.article
                         , mostReadArticles = fold props.mostReadArticles
                         , latestArticles = fold props.latestArticles
                         , staticPage = map StaticPageResponse $
                                        { pageName:_, pageContent:_, pageScript: initialValues.staticPageScript }
                                        <$> props.staticPageName
                                        <*> initialValues.staticPageContent
                         , categoryStructure = props.categoryStructure
                         , showAds = not props.globalDisableAds && initialValues.state.showAds
                         , catMap = initialCatMap
                         , frontpageFeeds = map ({stamp: initialValues.startTime, feed: _}) props.initialFrontpageFeed
                         , route = fromMaybe Routes.Frontpage $ hush $
                                   match (Routes.routes initialCatMap) initialPath
                         , user = Nothing
                         , ssrPreview = true
                         }

  -- For tests, they are prone to break in uninteresting ways with ads
  let setState = if not props.globalDisableAds
                 then setState_
                 else \f -> setState_ $ \s -> (f s) { showAds = false }

  let loadArticle articleId = Aff.launchAff_ do
        case UUID.parseUUID articleId of
          Nothing -> liftEffect $ setState _ { article = Just $ Left unit }
          Just uuid -> do
            liftEffect $ setTitle "Laddar..."
            liftEffect $ setState _ { article = Nothing }
            eitherArticle <- Lettera.getArticleAuth uuid mosaicoPaper
            liftEffect case eitherArticle of
              Right a@{ article } -> do
                liftEffect $ setTitle article.title
                Article.evalEmbeds article
                sendArticleAnalytics article $ join state.user
                randomAdvertorial <- pickRandomElement $ fromMaybe [] state.advertorials
                setState _
                  { article = Just $ Right a
                  , showAds = not article.removeAds && not (article.articleType == Advertorial)
                  , ssrPreview = false
                  , singleAdvertorial = randomAdvertorial
                  }
              Left _ -> do
                liftEffect $ setTitle "Något gick fel"
                setState _ { article = Just $ Left unit }

  useEffectOnce do
    withLoginLock <- (\l -> Aff.bracket (Aff.AVar.put unit l)
                            (const $ Aff.AVar.take l) <<< const) <$> AVar.empty
    alreadySentInitialAnalytics <- AVar.new false
    let initialSendAnalytics = case props.article of
          Just a -> sendArticleAnalytics a.article
          Nothing -> const $ pure unit
    giveUpLogin <- Aff.killFiber (Exception.error "give up login") <$> Aff.launchAff do
      Aff.delay $ Milliseconds 2000.0
      withLoginLock do
        liftEffect $ setState _ { user = Just Nothing }
        liftEffect $ initialSendAnalytics Nothing
        _ <- Aff.AVar.take alreadySentInitialAnalytics
        Aff.AVar.put true alreadySentInitialAnalytics

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
      -- magicLogin doesn't actually call the callback if it fails
      magicLogin Nothing $ hush >>> \u -> Aff.launchAff_ $ withLoginLock do
        giveUpLogin
        liftEffect $ do
          setState _ { user = Just u }
          state.logger.setUser u
        alreadySent <- Aff.AVar.take alreadySentInitialAnalytics
        when (not alreadySent) $ liftEffect $ initialSendAnalytics u
      advertorials <- Lettera.responseBody <$> Lettera.getAdvertorials mosaicoPaper
      randomAdvertorial <- liftEffect $ pickRandomElement $ fromMaybe [] advertorials
      liftEffect $ setState \s -> s
        { advertorials = advertorials
        , singleAdvertorial = s.singleAdvertorial <|> randomAdvertorial
        }
    pure $ Aff.launchAff_ giveUpLogin

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
          Nothing -> do
            Aff.launchAff_ $ loadFeed feedName
            setState _ { showAds = true }
          Just { stamp } -> do
            now <- Now.nowDateTime
            when (DateTime.diff now stamp > maxAge) do
              setState \s -> s { frontpageFeeds = HashMap.delete feedName s.frontpageFeeds, showAds = true }
              Aff.launchAff_ $ loadFeed feedName
      onPaywallEvent = do
        maybe (pure unit) loadArticle $ _.article.uuid <$> (join <<< map hush $ state.article)

  useEffect (state.route /\ map _.cusno (join state.user)) do
    -- When the route has changed, let's set `goingForward` to `false`, as we use this value
    -- to recognize should the page be scrolled to the top or not. When one moves backwards in browser history,
    -- the page should be in the same y position it was when `goingForward`
    initialValues.nav.replaceState (JSON.write { goingForward: false }) <<< _.pathname =<< initialValues.nav.locationState

    case state.route of
      Routes.Frontpage -> setFrontpage (CategoryFeed frontpageCategoryLabel)
      Routes.TagPage tag -> setFrontpage (TagFeed tag)
      Routes.SearchPage Nothing -> pure unit
      Routes.SearchPage (Just query) -> setFrontpage (SearchFeed query)
      -- Always uses server side provided article
      Routes.DraftPage -> setState _  { showAds = false }
      Routes.ProfilePage -> pure unit
      Routes.ArticlePage articleId
        | Just article <- map _.article (join $ map hush $ state.article)
        , articleId == article.uuid
        -> do
          when (state.ssrPreview && _.premium article) $
            loadArticle articleId
          setState _ { showAds = not article.removeAds && not (article.articleType == Advertorial) }
        | otherwise -> loadArticle articleId
      Routes.MenuPage -> setState _ { showAds = false }
      Routes.NotFoundPage _path -> setState _ { showAds = true }
      Routes.CategoryPage (Category c) -> setFrontpage (CategoryFeed c.label)
      Routes.EpaperPage -> setState _ { showAds = true }
      Routes.StaticPage page
        | Just (StaticPageResponse r) <- state.staticPage
        , r.pageName == page
        -> when (isJust state.prevRoute) do
             foldMap (\p -> evalExternalScripts [ScriptTag $ "<script>" <> p <> "</script>"]) r.pageScript
             setState _ { showAds = false }
        | otherwise ->
          Aff.launchAff_ do
            staticPage <- fetchStaticPage page
            liftEffect $ setState _  { staticPage = Just staticPage, showAds = false }
            case staticPage of
              StaticPageResponse r
                | Just p <- r.pageScript -> liftEffect $ evalExternalScripts [ScriptTag $ "<script>" <> p <> "</script>"]
              _ -> mempty
      Routes.DebugPage _ -> pure unit
      Routes.DeployPreview -> liftEffect $ setState _  { route = Routes.Frontpage }

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

    case state.route of
      Routes.Frontpage -> setTitle $ Paper.paperName mosaicoPaper
      Routes.TagPage tag -> setTitle $ unwrap tag
      Routes.SearchPage _ -> setTitle "Sök"
      Routes.ProfilePage -> setTitle "Min profil"
      Routes.MenuPage -> setTitle "Meny"
      Routes.NotFoundPage _ -> setTitle "Oops... 404"
      Routes.CategoryPage (Category c) -> setTitle $ unwrap c.label
      Routes.EpaperPage -> setTitle "E-Tidningen"
      Routes.StaticPage page -> setTitle page
      _ -> pure unit

    pure mempty

  pure $ render setState state initialValues.components initialValues.nav onPaywallEvent

scrollToTop :: Effect Unit
scrollToTop = Web.scroll 0 0 =<< Web.window

pickRandomElement :: forall a. Array a -> Effect (Maybe a)
pickRandomElement [] = pure Nothing
pickRandomElement elements = do
  randomIndex <- randomInt 0 (length elements - 1)
  pure $ index elements randomIndex

routeListener :: Categories -> ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener c setState oldLoc location
  -- If the prev and current route are the same, let's not do anything.
  -- The routeListener gets triggered twice, as we are replacing the route state
  -- to set `goingForward = false` after every route switch
  | maybe false ((_ == location.pathname) <<< _.pathname) oldLoc = mempty
  | otherwise = do
      runEffectFn1 refreshAdsImpl ["mosaico-ad__top-parade", "mosaico-ad__parade"]

      let newRoute = match (Routes.routes c) $ Routes.stripFragment $ location.pathname <> location.search
          locationState = hush $ JSON.read location.state
          oldPath = maybe "" (\l -> l.pathname <> l.search) oldLoc

      -- When we navigate the site, let's scroll to top of the page whenever we are `goingForward`
      foldMap (\(s :: Routes.RouteState) -> when s.goingForward scrollToTop) locationState

      -- Let's always scroll to top with article pages, as the behaviour of going back in
      -- browser history is a bit buggy currently. This is because each time we land on an article page,
      -- the page is basically blank, so the browser loses the position anyway (there's nothing to recover to).
      -- If we want to fix this, we'd have to keep prev article in state too.
      foldMap (case _ of Routes.ArticlePage _ -> scrollToTop
                         _                    -> mempty)
              (hush newRoute)

      case newRoute of
        Right path -> do
          setState \s -> s { route = path
                           , prevRoute = Just (Tuple s.route oldPath)
                           , clickedArticle = case path of
                               Routes.ArticlePage articleId
                                 | Just articleId /= (_.uuid <$> s.clickedArticle) -> Nothing
                               _ -> s.clickedArticle
                           }
          case path of
            Routes.ArticlePage _ -> pure unit
            _                    -> sendPageView
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
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "mosaico"
  logger.setTag "paper" _mosaicoPaper

  loginModalComponent <- LoginModal.loginModal
  searchComponent     <- Search.searchComponent
  webviewComponent    <- Webview.webviewComponent
  articleComponent    <- Article.component
  epaperComponent     <- Epaper.component
  imageComponent      <- Image.component
  headerComponent     <- Header.component
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
        , staticPage: Nothing
        , categoryStructure: []
        , catMap: Map.empty
        , frontpageFeeds: HashMap.empty
        , showAds: true
        , ssrPreview: true
        , advertorials: Nothing
        , singleAdvertorial: Nothing
        , logger
        }
    , components:
        { loginModalComponent
        , searchComponent
        , webviewComponent
        , articleComponent
        , epaperComponent
        , imageComponent
        , headerComponent
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
      globalDisableAds = fromMaybe false $ toMaybe jsProps.globalDisableAds
      staticPageName = toMaybe jsProps.staticPageName
      -- Decoding errors are being hushed here, although if this
      -- comes from `window.categoryStructure`, they should be
      -- valid categories
      categoryStructure = foldMap (mapMaybe (hush <<< decodeJson)) $ toMaybe jsProps.categoryStructure
  in { article, mostReadArticles, latestArticles, initialFrontpageFeed, staticPageName, categoryStructure, globalDisableAds }

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
               setState _ { modalView = Nothing, user = Just $ Just u }
               state.logger.setUser $ Just u
               onPaywallEvent
             Left _err -> do
               onPaywallEvent
               -- TODO: Handle properly
               Console.error $ "Login error " <> show _err
        , onClose: setState \s -> s { modalView = Nothing }
        }
    _ -> mempty
  <> renderRouteContent state.route
  where
    renderRouteContent = case _ of
       Routes.CategoryPage category -> renderCategory category
       Routes.ArticlePage articleId
         | Just (Right fullArticle@{ article }) <- state.article -> mosaicoLayoutNoAside $
           if article.uuid == articleId
           -- If we have this article already in `state`, let's render it
           then
             case article.articleType of
               Advertorial
                 | elem "Basic" article.categories
                 -> Advertorial.Basic.render components.imageComponent { article, imageProps: Nothing, advertorialClassName: Nothing }
                 | elem "Standard" article.categories
                 -> Advertorial.Standard.render components.imageComponent { article }
                 -- In a case we can't match the category of an advertorial article
                 -- let's show it as a "Basic" advertorial, rather than a regular article
                 | otherwise -> Advertorial.Basic.render components.imageComponent { article, imageProps: Nothing, advertorialClassName: Nothing }
               _ -> renderArticle (Right fullArticle)
           else loadingSpinner
         | Just stub <- state.clickedArticle -> mosaicoLayoutNoAside $ renderArticle $ Left stub
         | Nothing <- state.article -> mosaicoLayoutNoAside loadingSpinner
         | otherwise -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.Frontpage -> renderFrontpage
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
              label = Just $ "Sökresultat: " <> queryString
          in frontpage (Just header) label frontpageArticles
       Routes.NotFoundPage _ -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.TagPage tag ->
         let maybeFeed = _.feed <$> HashMap.lookup (TagFeed tag) state.frontpageFeeds
          in case maybeFeed of
               Just (ArticleList tagFeed)
                 | null tagFeed -> mosaicoDefaultLayout Error.notFoundWithAside
               _                -> frontpageNoHeader Nothing maybeFeed
       Routes.MenuPage ->
         flip (mosaicoLayout "menu-open") false
         $ Menu.render
             { changeRoute: Routes.changeRoute router
             , categoryStructure: state.categoryStructure
             , onCategoryClick
             , user: state.user
             , onLogin
             , onLogout
             }
       Routes.EpaperPage -> mosaicoLayoutNoAside
         $ components.epaperComponent
             { user: state.user
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
       Routes.DraftPage ->
         maybe
           (mosaicoLayoutNoAside $ renderArticle $ Right notFoundArticle)
           (renderRouteContent <<< Routes.ArticlePage <<< _.uuid <<< _.article)
           $ join <<< map hush $ state.article
       Routes.StaticPage _ -> mosaicoLayoutNoAside $ case state.staticPage of
         Nothing -> DOM.text "laddar"
         Just (StaticPageResponse page)  ->
           DOM.div { className: "mosaico--static-page", dangerouslySetInnerHTML: { __html: page.pageContent } }
         Just StaticPageNotFound -> Error.notFoundWithAside
         Just StaticPageOtherError -> Error.somethingWentWrong
       Routes.DebugPage _ -> frontpageNoHeader Nothing $ _.feed <$> HashMap.lookup (CategoryFeed $ CategoryLabel "debug") state.frontpageFeeds
       -- NOTE: This should not ever happen, as we always "redirect" to Frontpage route from DeployPreview
       Routes.DeployPreview -> renderFrontpage
    renderFrontpage = maybe mempty renderCategory $ Map.lookup frontpageCategoryLabel state.catMap

    renderCategory :: Category -> JSX
    renderCategory category@(Category c) =
      let maybeFeed = _.feed <$> HashMap.lookup (CategoryFeed c.label) state.frontpageFeeds
      in case c.type of
        Webview -> mosaicoLayoutNoAside $ components.webviewComponent { category }
        Link -> mempty -- TODO
        Prerendered -> maybe (mosaicoLayoutNoAside loadingSpinner) (frontpageNoHeader Nothing <<< Just) maybeFeed
        Feed -> frontpageNoHeader (Just c.label) maybeFeed

    frontpageNoHeader :: Maybe CategoryLabel -> Maybe ArticleFeed -> JSX
    frontpageNoHeader = frontpage Nothing <<< map unwrap

    frontpage :: Maybe JSX -> Maybe String -> Maybe ArticleFeed -> JSX
    frontpage maybeHeader maybeCategorLabel (Just (ArticleList list)) = listFrontpage maybeHeader maybeCategorLabel $ Just list
    frontpage maybeHeader _ (Just (Html html))                        = prerenderedFrontpage maybeHeader $ Just html
    frontpage maybeHeader _ _                                         = listFrontpage maybeHeader Nothing Nothing

    listFrontpage :: Maybe JSX -> Maybe String -> Maybe (Array ArticleStub) -> JSX
    listFrontpage maybeHeader label content = mosaicoDefaultLayout $
      (fromMaybe mempty maybeHeader) <>
      (Frontpage.render $ Frontpage.List
        { label
        , content
        , onArticleClick
        , onTagClick
        })

    prerenderedFrontpage :: Maybe JSX -> Maybe String -> JSX
    prerenderedFrontpage maybeHeader content =
      mosaicoLayout "" inner false
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
    hooks = [ Frontpage.RemoveTooltips, Frontpage.MostRead state.mostReadArticles onClickHandler
            , Frontpage.Latest state.latestArticles onClickHandler
            , Frontpage.ArticleUrltoRelative
            , Frontpage.Ad "Box Ad 1 DESKTOP" "mosaico-ad__box"
            , Frontpage.Ad "Box Ad 2 DESKTOP" "mosaico-ad__box1"
            , Frontpage.Ad "Box Ad 3 DESKTOP" "mosaico-ad__box2"
            , Frontpage.Ad "Box Ad 4 DESKTOP" "mosaico-ad__box3"
            , Frontpage.Ad "Ad 1"             "mosaico-ad__bigbox1"
            , Frontpage.Ad "Ad 2"             "mosaico-ad__bigbox2"
            ]

    mosaicoDefaultLayout :: JSX -> JSX
    mosaicoDefaultLayout content = mosaicoLayout "" content true

    mosaicoLayoutNoAside :: JSX -> JSX
    mosaicoLayoutNoAside content = mosaicoLayout "" content false

    mosaicoLayout :: String -> JSX -> Boolean -> JSX
    mosaicoLayout extraClasses content showAside = DOM.div_
      [ guard state.showAds Mosaico.ad { contentUnit: "mosaico-ad__top-parade" }
      , DOM.div
          { className: "mosaico grid " <> extraClasses
          , id: Paper.toString mosaicoPaper
          , children:
              [ Header.topLine
              , components.headerComponent
                  { changeRoute: Routes.changeRoute router
                  , categoryStructure: state.categoryStructure
                  , catMap: state.catMap
                  , onCategoryClick
                  , user: state.user
                  , onLogin
                  , onProfile
                  , onStaticPageClick
                  , onMenuClick:
                      case state.route of
                        Routes.MenuPage
                          | Just prevRoute <- state.prevRoute
                          -> Routes.changeRoute router $ snd prevRoute
                        _ -> Routes.changeRoute router "/meny"
                  }
              , Header.mainSeparator
              , guard state.showAds Mosaico.ad { contentUnit: "mosaico-ad__parade" }
              , content
              , footer mosaicoPaper onStaticPageClick
              , guard showAside $ DOM.aside
                  { className: "mosaico--aside"
                  , children:
                      [ guard state.showAds Mosaico.ad { contentUnit: "mosaico-ad__box" }
                      , MostReadList.render
                          { mostReadArticles: state.mostReadArticles
                          , onClickHandler
                          }
                      , guard state.showAds Mosaico.ad { contentUnit: "mosaico-ad__box1" }
                      , LatestList.render
                          { latestArticles: state.latestArticles
                          , onClickHandler
                          }
                      ] <> guard state.showAds
                      [ Mosaico.ad { contentUnit: "mosaico-ad__box2" }
                      , Mosaico.ad { contentUnit: "mosaico-ad__box3" }
                      , Mosaico.ad { contentUnit: "mosaico-ad__box4" }
                      , Mosaico.ad { contentUnit: "mosaico-ad__box5" }
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
        , advertorial: state.singleAdvertorial
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
        _ -> capture_ $ Routes.changeRoute router ("/sida/" <> link)

    onLogin = capture_ $ setState \s -> s { modalView = Just LoginModal }

    onLogout = capture_ do
      Aff.launchAff_ $ logout $ const $ pure unit
      setState _ { user = Just Nothing }
      state.logger.setUser Nothing
      onPaywallEvent

    -- Search is done via the router
    doSearch query = Routes.changeRoute router ("/sök?q=" <> query)

    simpleRoute = Routes.changeRoute router
