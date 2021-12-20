module Mosaico where

import Prelude

import Data.Argonaut.Core (Json, toArray, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapMaybe, null)
import Data.Either (Either(..), hush)
import Data.Foldable (fold, foldMap)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String as String
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign (unsafeFromForeign)
import KSF.Auth (enableCookieLogin) as Auth
import KSF.Paper as Paper
import KSF.User (User, magicLogin)
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category(..), CategoryLabel (..), FullArticle(..), Tag (..), isPreviewArticle, fromFullArticle, notFoundArticle, parseArticleStubWithoutLocalizing, parseArticleWithoutLocalizing, tagToURIComponent)
import Mosaico.Article as Article
import Mosaico.Error as Error
import Mosaico.Eval (ScriptTag(..), evalExternalScripts)
import Mosaico.Frontpage as Frontpage
import Mosaico.Header as Header
import Mosaico.Header.Menu as Menu
import Mosaico.LoginModal as LoginModal
import Mosaico.MostReadList as MostReadList
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Routes as Routes
import Mosaico.Search as Search
import Mosaico.StaticPage (StaticPageResponse(..), getInitialStaticPageContent, fetchStaticPage)
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
  { article :: Maybe FullArticle
  , mostReadArticles :: Array ArticleStub
  , route :: Routes.MosaicoPage
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , user :: Maybe User
  , staticPage :: Maybe StaticPageResponse
  , categoryStructure :: Array Category
  , frontpageFeeds :: HashMap ArticleFeed (Array ArticleStub)
  }

type SetState = (State -> State) -> Effect Unit

type Components =
  { loginModalComponent :: LoginModal.Props -> JSX
  , mostReadListComponent :: MostReadList.Props -> JSX
  , frontpageComponent :: Frontpage.Props -> JSX
  , searchComponent :: Search.Props -> JSX
  }

type Props =
  { article :: Maybe FullArticle
  , mostReadArticles :: Maybe (Array ArticleStub)
  , staticPageName :: Maybe String
  , categoryStructure :: Array Category
  , initialFrontpageFeed :: HashMap ArticleFeed (Array ArticleStub)
  , user :: Maybe User
  }
type JSProps =
  { article :: Nullable Json
  , isPreview :: Nullable Boolean
  , mostReadArticles :: Nullable (Array Json)
  , staticPageName :: Nullable String
  , categoryStructure :: Nullable (Array Json)
  , initialFrontpageFeed :: Nullable { feedType    :: Nullable String
                                     , feedPage    :: Nullable String
                                     , feedContent :: Nullable String
                                     }
  , user :: Nullable Json
  }

data ArticleFeed
  = CategoryFeed (Maybe CategoryLabel) -- `Nothing` represents root
  | TagFeed Tag
  | SearchFeed String
derive instance eqArticleFeed :: Eq ArticleFeed
instance showArticleFeed :: Show ArticleFeed where
  show (CategoryFeed c) = "CategoryFeed " <> show c
  show (TagFeed t) = "TagFeed" <> show t
  show (SearchFeed s) = "SearchFeed " <> s
instance hashableArticleFeed :: Hashable ArticleFeed where
  hash = hash <<< show

app :: Component Props
app = do
  Auth.enableCookieLogin
  initialValues <- getInitialValues
  component "Mosaico" $ mosaicoComponent initialValues

mosaicoComponent
  :: InitialValues
  -> Props
  -> Render Unit (UseEffect Routes.MosaicoPage (UseEffect Unit (UseState State Unit))) JSX
mosaicoComponent initialValues props = React.do
  state /\ setState <- useState initialValues.state
                         { article = props.article
                         , mostReadArticles = fold props.mostReadArticles
                         , staticPage = map StaticPageResponse $
                                        { pageName:_, pageContent:_, pageScript: Nothing }
                                        <$> props.staticPageName
                                        <*> initialValues.staticPageContent
                         , categoryStructure = props.categoryStructure
                         , frontpageFeeds = props.initialFrontpageFeed
                         , route = fromMaybe Routes.Frontpage $ hush $
                                   match (Routes.routes props.categoryStructure) initialValues.locationState.path
                         , user = props.user
                         }

  let loadArticle articleId = Aff.launchAff_ do
        case UUID.parseUUID articleId of
          Nothing -> liftEffect $ setState _ { article = Nothing }
          Just uuid -> do
            eitherArticle <- Lettera.getArticleAuth uuid
            liftEffect case eitherArticle of
              Right article -> do
                Article.evalEmbeds $ fromFullArticle article
                setState _ { article = Just article }
              Left _ -> setState _ { article = Nothing }

  useEffectOnce do
    Aff.launchAff_ do
      cats <- if null props.categoryStructure
              then Lettera.getCategoryStructure mosaicoPaper
              else pure props.categoryStructure
      liftEffect do
        setState _ { categoryStructure = cats }
        -- Listen for route changes and set state accordingly
        void $ locations (routeListener cats setState) initialValues.nav
      when (isNothing props.user) $
        magicLogin Nothing $ \u -> case u of
          Right user -> setState _ { user = Just user }
          _ -> pure unit
    pure mempty

  let setFrontpage feedName =
        case HashMap.lookup feedName state.frontpageFeeds of
          Nothing -> Aff.launchAff_ do
            let letteraFn =
                  case feedName of
                    TagFeed t -> Lettera.getByTag 0 20 t mosaicoPaper
                    CategoryFeed c -> Lettera.getFrontpage mosaicoPaper (map unwrap c)
                    SearchFeed q -> Lettera.search 0 20 mosaicoPaper q
            feed <- letteraFn
            liftEffect $ setState \s -> s { frontpageFeeds = HashMap.insert feedName feed s.frontpageFeeds }
          Just _ -> pure unit
      onPaywallEvent = do
        maybe (pure unit) loadArticle $ _.uuid <<< fromFullArticle <$> state.article

  useEffect state.route do
    case state.route of
      Routes.Frontpage -> setFrontpage (CategoryFeed Nothing)
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
      Routes.CategoryPage category -> setFrontpage (CategoryFeed (Just category))
      Routes.StaticPage page
        | Just (StaticPageResponse r) <- state.staticPage
        , r.pageName == page
        -> pure unit
        | otherwise ->
          Aff.launchAff_ do
            staticPage <- fetchStaticPage page
            liftEffect $ setState _  { staticPage = Just staticPage }  
            case staticPage of
              StaticPageResponse r
                | Just p <- r.pageScript -> liftEffect $ evalExternalScripts [ScriptTag $ "<script>" <> p <> "</script>"]
              _ -> mempty

    case props.mostReadArticles of
      Just mostReads
        | not $ null mostReads -> liftEffect $ setState \s -> s { mostReadArticles = mostReads }
      _ ->
        Aff.launchAff_ do
          mostReadArticles <- Lettera.getMostRead 0 10 "" mosaicoPaper true
          liftEffect $ setState \s -> s { mostReadArticles = mostReadArticles }

    pure mempty

  pure $ render setState state initialValues.components initialValues.nav onPaywallEvent

routeListener :: Array Category -> ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener c setState _oldLoc location = do
  case match (Routes.routes c) location.path of
    Right path -> setState _ { route = path }
    Left _     -> pure unit

type InitialValues =
  { state :: State
  , components :: Components
  , nav :: PushStateInterface
  , locationState :: LocationState
  , staticPageContent :: Maybe String
  }

getInitialValues :: Effect InitialValues
getInitialValues = do
  nav <- makeInterface
  locationState <- nav.locationState
  staticPageContent <- toMaybe <$> getInitialStaticPageContent

  loginModalComponent <- LoginModal.loginModal
  mostReadListComponent <- MostReadList.mostReadListComponent
  frontpageComponent    <- Frontpage.frontpageComponent
  searchComponent       <- Search.searchComponent
  pure
    { state:
        { article: Nothing
        , mostReadArticles: []
        , route: Routes.Frontpage
        , clickedArticle: Nothing
        , modalView: Nothing
        , user: Nothing
        , staticPage: Nothing
        , categoryStructure: []
        , frontpageFeeds: HashMap.empty
        }
    , components:
        { loginModalComponent
        , mostReadListComponent
        , frontpageComponent
        , searchComponent
        }
    , nav
    , locationState
    , staticPageContent
    }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  let isPreview = fromMaybe false $ toMaybe jsProps.isPreview
      mkFullArticle
        | isPreview = PreviewArticle
        | otherwise = FullArticle
      article = mkFullArticle <$> (hush <<< parseArticleWithoutLocalizing =<< toMaybe jsProps.article)
      mostReadArticles = map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ toMaybe jsProps.mostReadArticles

      initialFrontpageFeed :: HashMap ArticleFeed (Array ArticleStub)
      initialFrontpageFeed = fromMaybe HashMap.empty do
        feed <- toMaybe jsProps.initialFrontpageFeed
        let feedPage = toMaybe feed.feedPage
        feedType <- do
          f <- toMaybe feed.feedType
          case String.toLower f of
            "categoryfeed" -> Just $ CategoryFeed (map CategoryLabel feedPage)
            "tagfeed"      -> map (TagFeed <<< Tag) feedPage
            "searchfeed"   -> SearchFeed <$> feedPage
            _              -> Nothing
        feedContent <- do
          content <- toMaybe feed.feedContent >>= (jsonParser >>> hush) >>= toArray
          pure $ mapMaybe (hush <<< parseArticleStubWithoutLocalizing) content
        pure $ HashMap.singleton feedType feedContent

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
       Routes.CategoryPage category ->
         mosaicoDefaultLayout $ components.frontpageComponent
           { frontpageArticles: HashMap.lookup (CategoryFeed (Just category)) state.frontpageFeeds
           , onArticleClick
           , onTagClick
           }
       Routes.ArticlePage articleId
         | Just fullArticle <- state.article
         , article <- fromFullArticle fullArticle
         -- If we have this article already in `state`, let's pass that to `articleComponent`
         , article.uuid == articleId -> mosaicoLayoutNoAside $ renderArticle (Right fullArticle)
         | Just stub <- state.clickedArticle -> mosaicoLayoutNoAside $ renderArticle $ Left stub
         | otherwise -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.Frontpage -> frontpage $ HashMap.lookup (CategoryFeed Nothing) state.frontpageFeeds
       Routes.SearchPage Nothing ->
          mosaicoDefaultLayout $ components.searchComponent { query: Nothing, doSearch, searching: false, noResults: false }
       Routes.SearchPage query@(Just queryString) ->
          let frontpageArticles = HashMap.lookup (SearchFeed queryString) state.frontpageFeeds
              searching = isNothing frontpageArticles
              noResults = (null <$> frontpageArticles) == Just true
          in mosaicoDefaultLayout $
            components.searchComponent { query, doSearch, searching, noResults } <>
            components.frontpageComponent { frontpageArticles, onArticleClick, onTagClick }
       Routes.NotFoundPage _ -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.TagPage tag ->
         case HashMap.lookup (TagFeed tag) state.frontpageFeeds of
           Just tagFeed
             | not $ null tagFeed -> frontpage $ Just tagFeed
             | otherwise -> mosaicoDefaultLayout Error.notFoundWithAside
           Nothing -> frontpage Nothing
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
  where
    mosaicoDefaultLayout :: JSX -> JSX
    mosaicoDefaultLayout = flip mosaicoLayout true

    mosaicoLayoutNoAside :: JSX -> JSX
    mosaicoLayoutNoAside = flip mosaicoLayout false

    mosaicoLayout :: JSX -> Boolean -> JSX
    mosaicoLayout content showAside = DOM.div
      { className: "mosaico grid"
      , id: Paper.toString mosaicoPaper
      , children:
          [ Header.topLine
          , Header.render
              { router
              , categoryStructure: state.categoryStructure
              , onCategoryClick
              , user: state.user
              , onLogin
              }
          , Header.mainSeparator
          , content
          , DOM.footer
              { className: "mosaico--footer"
              , children: [ DOM.text "footer" ]
              }
          , guard showAside $ DOM.aside
              { className: "mosaico--aside"
              , children:
                  [ components.mostReadListComponent
                      { mostReadArticles: state.mostReadArticles
                      , onClickHandler: \articleStub -> do
                          setState _ { clickedArticle = Just articleStub }
                          void $ Web.scroll 0 0 =<< Web.window
                          router.pushState (write {}) $ "/artikel/" <> articleStub.uuid
                      }
                  ]
              }
          ]
      }

    onCategoryClick (Category c) =
      case state.route of
        Routes.CategoryPage category | category == c.label -> mempty
        _ -> capture_ do
          void $ Web.scroll 0 0 =<< Web.window
          router.pushState (write {}) $ "/" <> show c.label

    onTagClick tag = capture_ do
      void $ Web.scroll 0 0 =<< Web.window
      router.pushState (write {}) $ "/tagg/" <> tagToURIComponent tag

    onArticleClick article = capture_ do
      setState _ { clickedArticle = Just article }
      void $ Web.scroll 0 0 =<< Web.window
      router.pushState (write {}) $ "/artikel/" <> article.uuid

    onLogin = setState \s -> s { modalView = Just LoginModal }

    -- Search is done via the router
    doSearch query = do
      router.pushState (write {}) $ "/sök?q=" <> query

    frontpage frontpageArticles = mosaicoDefaultLayout $ components.frontpageComponent
      { frontpageArticles
      , onArticleClick
      , onTagClick
      }

    renderArticle :: Either ArticleStub FullArticle -> JSX
    renderArticle article =
      Article.render
        { paper: mosaicoPaper
        , article
        , onLogin
        , user: state.user
        , onPaywallEvent
        , onTagClick
        , onArticleClick
        }
