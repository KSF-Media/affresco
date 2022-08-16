module Mosaico where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (toArray, toBoolean, toString) as JSON
import Data.Argonaut.Decode (decodeJson)
import Data.Array (catMaybes, find, index, length, mapMaybe, null)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap, elem)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String.Regex (match, regex) as Regex
import Data.Int (ceil)
import Data.Nullable (Nullable, toMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple (..), fst)
import Data.UUID as UUID
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff as Aff
import Effect.Aff.AVar as Aff.AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Random (randomInt)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import KSF.Auth (enableCookieLogin) as Auth
import KSF.Paper as Paper
import KSF.Sentry as Sentry
import KSF.Spinner (loadingSpinner)
import KSF.User (User, logout, magicLogin)
import KSF.User.Cusno (Cusno)
import Lettera as Lettera
import Lettera.Models (ArticleStub, ArticleType(..), Categories, Category(..), CategoryLabel(..), CategoryType(..), FullArticle, articleToArticleStub, categoriesMap, frontpageCategoryLabel, notFoundArticle, parseArticleStubWithoutLocalizing, parseArticleWithoutLocalizing, readArticleType, tagToURIComponent)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.Analytics (sendArticleAnalytics, sendPageView)
import Mosaico.Article as Article
import Mosaico.Article.Advertorial.Basic as Advertorial.Basic
import Mosaico.Article.Advertorial.Standard as Advertorial.Standard
import Mosaico.Cache as Cache
import Mosaico.Epaper as Epaper
import Mosaico.Error as Error
import Mosaico.Eval (ScriptTag(..), evalExternalScripts)
import Mosaico.Feed as Feed
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), JSInitialFeed, parseFeed)
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
import Simple.JSON (read) as JSON
import Web.HTML (window) as Web
import Web.HTML.History (back) as Web
import Web.HTML.HTMLDocument (setTitle) as Web
import Web.HTML.Window (document, history, scroll) as Web
import KSF.Driver (setDriver)
import Affjax.Web (driver)

foreign import refreshAdsImpl :: EffectFn1 (Array String) Unit
foreign import sentryDsn_ :: Effect String
foreign import setManualScrollRestoration :: Effect Unit

data ModalView = LoginModal

type State =
  { article :: Maybe (Either Unit FullArticle)
  , route :: Routes.MosaicoPage
  , prevRoute :: Maybe (Tuple Routes.MosaicoPage String)
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , user :: Maybe (Maybe User)
  , staticPage :: Maybe StaticPageResponse
  , categoryStructure :: Array Category
  , catMap :: Categories
  , feeds :: HashMap ArticleFeedType ArticleFeed
  , ssrPreview :: Boolean
  , advertorials :: Maybe (Array ArticleStub)
  , singleAdvertorial :: Maybe ArticleStub
  , logger :: Sentry.Logger
  , scrollToYPosition :: Maybe Number
  , starting :: Boolean
  }

type SetState = (State -> State) -> Effect Unit

type Components =
  { loginModalComponent :: LoginModal.Props -> JSX
  , searchComponent :: Search.Props -> JSX
  , webviewComponent :: Webview.Props -> JSX
  , articleComponent :: Article.Props -> JSX
  , epaperComponent :: Epaper.Props -> JSX
  , basicAdvertorialComponent :: Advertorial.Basic.Props -> JSX
  , standardAdvertorialComponent :: Advertorial.Standard.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  }

type Props =
  { article :: Maybe FullArticle
  , staticPageName :: Maybe String
  , categoryStructure :: Array Category
  -- For tests, they are prone to break in uninteresting ways with ads
  , globalDisableAds :: Boolean
  , initialFeeds :: Array (Tuple ArticleFeedType ArticleFeed)
  , headless :: Boolean
  }

type JSProps =
  { article :: Json
  , articleType :: Json
  , mostReadArticles :: Json
  , latestArticles :: Json
  , staticPageName :: Json
  , categoryStructure :: Json
  , globalDisableAds :: Json
  , initialFrontpageFeed :: Nullable JSInitialFeed
  , initialBreakingNews :: Nullable String
  , headless :: Json
  }

app :: Component Props
app = do
  Auth.enableCookieLogin
  setDriver driver
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
  state /\ setState <- useState initialValues.state
                         { article = Right <$> props.article
                         , clickedArticle = articleToArticleStub <<< _.article <$> props.article
                         , staticPage = map StaticPageResponse $
                                        { pageName:_, pageContent:_, pageScript: initialValues.staticPageScript }
                                        <$> props.staticPageName
                                        <*> initialValues.staticPageContent
                         , categoryStructure = props.categoryStructure
                         , catMap = initialCatMap
                         , feeds = HashMap.fromArray props.initialFeeds
                         , route = fromMaybe Routes.Frontpage $ hush $
                                   match (Routes.routes initialCatMap) initialPath
                         , user = Nothing
                         , ssrPreview = true
                         }

  let setFeed feed content =
        liftEffect $ setState $ \s -> s { feeds = HashMap.insert feed content s.feeds }
      loadArticle articleId withAnalytics = Aff.launchAff_ do
        case UUID.parseUUID articleId of
          Nothing -> liftEffect $ setState _ { article = Just $ Left unit }
          Just uuid -> do
            liftEffect $ setTitle "Laddar..."
            liftEffect $ setState _ { article = Nothing }
            cache <- Aff.AVar.read initialValues.cache
            eitherArticle <- Cache.parallelWithCommonActions cache setFeed $
                             Lettera.getArticleAuth uuid mosaicoPaper
            liftEffect case eitherArticle of
              Right a@{ article } -> do
                liftEffect $ setTitle article.title
                Article.evalEmbeds article
                when withAnalytics $
                  sendArticleAnalytics article $ join state.user
                randomAdvertorial <- pickRandomElement $ fromMaybe [] state.advertorials
                setState _
                  { article = Just $ Right a
                  , ssrPreview = false
                  , singleAdvertorial = randomAdvertorial
                  }
              Left _ -> do
                liftEffect $ setTitle "Något gick fel"
                setState _ { article = Just $ Left unit }

  useEffectOnce do
    setManualScrollRestoration
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
    void $ Cache.initClientCache props.initialFeeds >>= flip AVar.tryPut initialValues.cache
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

  let setFrontpage feedName = Aff.launchAff_ do
        -- In SPA mode, this may be called before catMap has been
        -- populated to state.  Synchronize with an AVar.
        catMap <- Aff.AVar.read initialValues.catMap
        cache <- Aff.AVar.read initialValues.cache
        fresh <- Cache.isFresh cache catMap feedName
        when (not fresh) $ liftEffect $
          setState $ \s -> s { feeds = HashMap.delete feedName s.feeds }
        foldMap (Cache.parallelLoadFeeds cache setFeed) $
          (map <<< map) (Tuple feedName) $ Cache.loadFeed cache catMap feedName
      onPaywallEvent = do
        maybe (pure unit) (\u -> loadArticle u true) $ _.article.uuid <$> (join <<< map hush $ state.article)

  useEffect (state.route /\ map _.cusno (join state.user)) do
    case state.route of
      Routes.Frontpage -> setFrontpage (CategoryFeed frontpageCategoryLabel)
      Routes.TagPage tag -> setFrontpage (TagFeed tag)
      Routes.SearchPage (Just query) -> setFrontpage (SearchFeed query)
      Routes.ArticlePage articleId
        | Just article <- map _.article (join $ map hush state.article)
        , articleId == article.uuid
        -> do
          when (state.ssrPreview && _.premium article) do
            -- When we're in a SSR premium article, don't send analytics events
            -- We already get it when trying to resolve authed user on startup
            loadArticle articleId false
        | otherwise -> loadArticle articleId true
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
      Routes.DeployPreview -> liftEffect $ setState _  { route = Routes.Frontpage }
      _ -> pure unit

    case state.route of
      Routes.Frontpage -> setTitle $ Paper.paperName mosaicoPaper
      Routes.TagPage tag -> setTitle $ unwrap tag
      Routes.SearchPage _ -> setTitle "Sök"
      Routes.ProfilePage -> setTitle "Min profil"
      Routes.MenuPage -> setTitle "Meny"
      Routes.NotFoundPage _ -> setTitle "Oops... 404"
      Routes.CategoryPage (Category c) -> setTitle $ unwrap c.label
      Routes.EpaperPage -> setTitle "E-Tidningen"
      Routes.StaticPage page -> setTitle (staticPageTitle page)
      _ -> pure unit


    let scrollToYPos y = Web.window >>= Web.scroll 0 y
    case state of
      -- User may have already started scrolling while content is loading
      { starting: true }              -> setState _ { starting = false }
      -- Let's always scroll to top with article pages, as the behaviour of going back in
      -- browser history is a bit buggy currently. This is because each time we land on an article page,
      -- the page is basically blank, so the browser loses the position anyway (there's nothing to recover to).
      -- If we want to fix this, we'd have to keep prev article in state too.
      { route: Routes.ArticlePage _ } -> scrollToYPos 0
      { scrollToYPosition: Just y }   -> scrollToYPos (ceil y)
      { scrollToYPosition: Nothing }  -> scrollToYPos 0

    pure mempty

  pure $ render props setState state initialValues.components initialValues.nav onPaywallEvent

staticPageTitle :: String -> String
staticPageTitle page =
  case page of
    "anslagstavlan"   -> "Anslagstavlan"
    "bruksvillkor"    -> "Bruksvillkor"
    "fiskecupen"      -> "Fiskecupen"
    "fragor-och-svar" -> "Frågor och svar"
    "insandare"       -> "Insändare"
    "kontakt"         -> "Kontakta oss"
    "kundservice"     -> "Kundservice"
    "tipsa-oss"       -> "Tipsa oss"
    _                 -> Paper.paperName mosaicoPaper

pickRandomElement :: forall a. Array a -> Effect (Maybe a)
pickRandomElement [] = pure Nothing
pickRandomElement elements = do
  randomIndex <- randomInt 0 (length elements - 1)
  pure $ index elements randomIndex

routeListener :: Categories -> ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener c setState oldLoc location = do
  runEffectFn1 refreshAdsImpl ["mosaico-ad__top-parade", "mosaico-ad__parade"]

  let newRoute = match (Routes.routes c) $ Routes.stripFragment $ location.pathname <> location.search
      (locationState :: Maybe Routes.RouteState) = hush $ JSON.read location.state
      oldPath = maybe "" (\l -> l.pathname <> l.search) oldLoc

  -- If the location we moved to was previously visited, let's scroll to the last y position it had.
  -- Note that we cannot scroll the page yet, but we need to do it via Mosaico's state
  -- as the content is rendered that way too (the page is empty at this point, or showing
  -- old content)
  let setYPosition { yPositionOnLeave: Just y } = setState _ { scrollToYPosition = Just y }
      setYPosition _ = setState _ { scrollToYPosition = Nothing }
  foldMap setYPosition locationState

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
  , catMap :: AVar.AVar Categories
  , cache :: AVar.AVar Cache.Cache
  }

getInitialValues :: Effect InitialValues
getInitialValues = do
  catMap <- AVar.empty
  cache <- AVar.empty
  nav <- makeInterface
  locationState <- nav.locationState
  staticPageContent <- toMaybe <$> getInitialStaticPageContent
  staticPageScript <- toMaybe <$> getInitialStaticPageScript
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "mosaico"
  logger.setTag "paper" _mosaicoPaper

  loginModalComponent          <- LoginModal.loginModal
  searchComponent              <- Search.searchComponent
  webviewComponent             <- Webview.webviewComponent
  articleComponent             <- Article.component
  epaperComponent              <- Epaper.component
  basicAdvertorialComponent    <- Advertorial.Basic.component
  standardAdvertorialComponent <- Advertorial.Standard.component
  headerComponent              <- Header.component
  pure
    { state:
        { article: Nothing
        , route: Routes.Frontpage
        , prevRoute: Nothing
        , clickedArticle: Nothing
        , modalView: Nothing
        , user: Nothing
        , staticPage: Nothing
        , categoryStructure: []
        , catMap: Map.empty
        , feeds: HashMap.empty
        , ssrPreview: true
        , advertorials: Nothing
        , singleAdvertorial: Nothing
        , logger
        , scrollToYPosition: Nothing
        , starting: true
        }
    , components:
        { loginModalComponent
        , searchComponent
        , webviewComponent
        , articleComponent
        , epaperComponent
        , basicAdvertorialComponent
        , standardAdvertorialComponent
        , headerComponent
        }
    , catMap
    , cache
    , nav
    , locationState
    , staticPageContent
    , staticPageScript
    }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  let article = { articleType: _, article: _ }
                <$> (readArticleType =<< JSON.toString jsProps.articleType)
                <*> (hush $ parseArticleWithoutLocalizing jsProps.article)
      initialFeeds =
        catMaybes
        [ parseFeed =<< toMaybe jsProps.initialFrontpageFeed
        , Tuple LatestFeed <<< ArticleList <$>
          (map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ JSON.toArray jsProps.latestArticles)
        , Tuple MostReadFeed <<< ArticleList <$>
          (map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ JSON.toArray jsProps.mostReadArticles)
        , Tuple BreakingNewsFeed <<< Html [] <$> toMaybe jsProps.initialBreakingNews
        ]
      globalDisableAds = fromMaybe false $ JSON.toBoolean jsProps.globalDisableAds
      staticPageName = JSON.toString jsProps.staticPageName
      -- Decoding errors are being hushed here, although if this
      -- comes from `window.categoryStructure`, they should be
      -- valid categories
      categoryStructure = foldMap (mapMaybe (hush <<< decodeJson)) $ JSON.toArray jsProps.categoryStructure
      headless = fromMaybe false $ JSON.toBoolean jsProps.headless
  in { article, initialFeeds, staticPageName, categoryStructure, globalDisableAds, headless }

jsApp :: Effect (React.ReactComponent JSProps)
jsApp = do
  Auth.enableCookieLogin
  setDriver driver
  initialValues <- getInitialValues
  React.reactComponent "Mosaico" $ mosaicoComponent initialValues <<< fromJSProps

render :: Props -> SetState -> State -> Components -> PushStateInterface -> Effect Unit -> JSX
render props setState state components router onPaywallEvent =
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
                 -> components.basicAdvertorialComponent { article, imageProps: Nothing, advertorialClassName: Nothing }
                 | elem "Standard" article.categories
                 -> components.standardAdvertorialComponent { article }
                 -- In a case we can't match the category of an advertorial article
                 -- let's show it as a "Basic" advertorial, rather than a regular article
                 | otherwise -> components.basicAdvertorialComponent { article, imageProps: Nothing, advertorialClassName: Nothing }
               _ -> renderArticle (Right fullArticle)
           else loadingSpinner
         | Just stub <- state.clickedArticle -> mosaicoLayoutNoAside $ renderArticle $ Left stub
         | Nothing <- state.article -> mosaicoLayoutNoAside loadingSpinner
         | otherwise -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.Frontpage -> renderFrontpage
       Routes.SearchPage Nothing ->
          mosaicoDefaultLayout $ components.searchComponent { query: Nothing, doSearch, searching: false }
       Routes.SearchPage query@(Just queryString) ->
          let frontpageArticles = HashMap.lookup (SearchFeed queryString) state.feeds
              searching = isNothing frontpageArticles
              noResults = case frontpageArticles of
                Just (ArticleList list)
                  | null list -> true
                _             -> false
              searchProps = { query, doSearch, searching }
              header = components.searchComponent searchProps
              label = if noResults
                      then Just "Inga resultat"
                      else Just $ "Sökresultat: " <> queryString
          in frontpage (Just header) label frontpageArticles
       Routes.NotFoundPage _ -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.TagPage tag ->
         let maybeFeed = HashMap.lookup (TagFeed tag) state.feeds
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
         Nothing -> loadingSpinner
         Just (StaticPageResponse page)  ->
           DOM.div { className: "mosaico--static-page", dangerouslySetInnerHTML: { __html: page.pageContent } }
         Just StaticPageNotFound -> Error.notFoundWithAside
         Just StaticPageOtherError -> Error.somethingWentWrong
       Routes.DebugPage _ -> frontpageNoHeader Nothing $ HashMap.lookup (CategoryFeed $ CategoryLabel "debug") state.feeds
       -- NOTE: This should not ever happen, as we always "redirect" to Frontpage route from DeployPreview
       Routes.DeployPreview -> renderFrontpage
    renderFrontpage = maybe mempty renderCategory $ Map.lookup frontpageCategoryLabel state.catMap

    renderCategory :: Category -> JSX
    renderCategory category@(Category c) =
      let maybeFeed = HashMap.lookup (CategoryFeed c.label) state.feeds
      in case c.type of
        Webview -> mosaicoLayoutNoAside $ components.webviewComponent { category }
        Link -> mempty -- TODO
        Prerendered -> maybe (mosaicoLayoutNoAside loadingSpinner) (frontpageNoHeader Nothing <<< Just) maybeFeed
        Feed -> frontpageNoHeader (Just c.label) maybeFeed

    frontpageNoHeader :: Maybe CategoryLabel -> Maybe ArticleFeed -> JSX
    frontpageNoHeader = frontpage Nothing <<< map unwrap

    frontpage :: Maybe JSX -> Maybe String -> Maybe ArticleFeed -> JSX
    frontpage maybeHeader maybeCategorLabel (Just (ArticleList list)) = listFrontpage maybeHeader maybeCategorLabel $ Just list
    frontpage maybeHeader _ (Just (Html list html))                   = prerenderedFrontpage maybeHeader list $ Just html
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

    prerenderedFrontpage :: Maybe JSX -> Array ArticleStub -> Maybe String -> JSX
    prerenderedFrontpage maybeHeader articles content =
      mosaicoLayout "" inner false
      where
        uuidRegex = hush $ Regex.regex "[^/]+$" mempty
        inner =
          (fromMaybe mempty maybeHeader) <>
          (Frontpage.render $ Frontpage.Prerendered
             { content
             , breakingNews
             , hooks
             , onClick: onFrontpageClick $
                \path -> do
                  let clickedArticle = do
                        regex <- uuidRegex
                        uuid <- NonEmptyArray.last =<< Regex.match regex path
                        find ((_ == uuid) <<< _.uuid) articles
                  setState _ { clickedArticle = clickedArticle } *> simpleRoute path
             })

    hooks :: Array Frontpage.Hook
    hooks = [ Frontpage.RemoveTooltips
            , Frontpage.MostRead (foldMap Feed.toList $ HashMap.lookup MostReadFeed state.feeds) onClickHandler
            , Frontpage.Latest (foldMap Feed.toList $ HashMap.lookup LatestFeed state.feeds) onClickHandler
            , Frontpage.ArticleUrltoRelative
            , Frontpage.EpaperBanner
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
    mosaicoLayout extraClasses content showAside =
      let header =
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
                     -- Confused state, got to go to somewhere but
                     -- not to menu again
                         | (fst <$> state.prevRoute) == Just Routes.MenuPage
                         -> Routes.changeRoute router "/"
                     -- Using changeRoute would overwrite the stored Y position
                         | Just _ <- state.prevRoute
                         -> Web.window >>= Web.history >>= Web.back
                     -- Don't know what else to do so might as well
                         | otherwise
                         -> Routes.changeRoute router "/"
                       _ -> Routes.changeRoute router "/meny"
                 , showHeading: case state.route of
                      Routes.ArticlePage _ -> false
                      Routes.StaticPage _ -> false
                      _ -> true
                 }
            ]
      in DOM.div_
        [ guard showAds Mosaico.ad { contentUnit: "mosaico-ad__top-parade", inBody: false }
        , DOM.div
            { className: "mosaico grid " <> extraClasses
            , id: Paper.toString mosaicoPaper
            , children:
                guard (not props.headless) header
                <>
                     [ guard showAds Mosaico.ad { contentUnit: "mosaico-ad__parade", inBody: false }
                     , content
                     , guard (not props.headless) (footer mosaicoPaper onStaticPageClick) --remember to hide footer if headless
                     , guard showAside $ DOM.aside
                         { className: "mosaico--aside"
                         , children:
                             [ guard showAds Mosaico.ad { contentUnit: "mosaico-ad__box", inBody: false }
                             , MostReadList.render
                                 { mostReadArticles
                                 , onClickHandler
                                 }
                             , guard showAds Mosaico.ad { contentUnit: "mosaico-ad__box1", inBody: false }
                             , LatestList.render
                                 { latestArticles
                                 , onClickHandler
                                 }
                             ] <> guard showAds
                             [ Mosaico.ad { contentUnit: "mosaico-ad__box2", inBody: false }
                             , Mosaico.ad { contentUnit: "mosaico-ad__box3", inBody: false }
                             , Mosaico.ad { contentUnit: "mosaico-ad__box4", inBody: false }
                             , Mosaico.ad { contentUnit: "mosaico-ad__box5", inBody: false }
                             ]
                         }
                     ]
            }
        ]

    showAds = not props.globalDisableAds && case state.route of
      Routes.Frontpage -> true
      Routes.TagPage _ -> true
      Routes.SearchPage _ -> true
      Routes.DraftPage -> false
      Routes.ProfilePage -> false
      Routes.ArticlePage _ -> case state.article of
        Nothing -> true
        Just eitherArticle -> case eitherArticle of
          Right { article: article} ->
            not article.removeAds && not (article.articleType == Advertorial)
          Left _ -> false
      Routes.MenuPage -> false
      Routes.NotFoundPage _ -> false
      Routes.CategoryPage _ -> true
      Routes.EpaperPage -> true
      Routes.StaticPage _ -> false
      Routes.DebugPage _ -> false
      Routes.DeployPreview -> false

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
        , mostReadArticles
        , latestArticles
        , advertorial: state.singleAdvertorial
        , breakingNews
        }

    onClickHandler articleStub = capture_ do
      setState _ { clickedArticle = Just articleStub }
      simpleRoute $ "/artikel/" <> articleStub.uuid

    onCategoryClick (Category { type: Webview }) =
      mempty
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

    latestArticles = foldMap Feed.toList $ HashMap.lookup LatestFeed state.feeds

    mostReadArticles = foldMap Feed.toList $ HashMap.lookup MostReadFeed state.feeds

    breakingNews = foldMap Feed.toHtml $ HashMap.lookup BreakingNewsFeed state.feeds
