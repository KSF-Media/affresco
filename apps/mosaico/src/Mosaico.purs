module Mosaico where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (mapMaybe, null)
import Data.Either (Either(..), either, hush)
import Data.Foldable (fold, foldMap)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Nullable (Nullable, toMaybe)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import KSF.Paper (Paper(..))
import KSF.User (User)
import Lettera as Lettera
import Lettera.Models (Article, ArticleStub, Category(..), FullArticle(..), fromFullArticle, notFoundArticle, parseArticleStubWithoutLocalizing, parseArticleWithoutLocalizing)
import Mosaico.Article as Article
import Mosaico.Error as Error
import Mosaico.Frontpage as Frontpage
import Mosaico.Header as Header
import Mosaico.Header.Menu as Menu
import Mosaico.LoginModal as LoginModal
import Mosaico.MostReadList as MostReadList
import Mosaico.Routes as Routes
import Mosaico.StaticPage (StaticPage, StaticPageResponse(..), fetchStaticPage)
import React.Basic (JSX)
import React.Basic.DOM as DOM
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
  , frontpageArticles :: Array ArticleStub
  , mostReadArticles :: Array ArticleStub
  , affArticle :: Maybe (Aff Article)
  , route :: Routes.MosaicoPage
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , articleComponent :: Article.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  , menuComponent :: Menu.Props -> JSX
  , loginModalComponent :: LoginModal.Props -> JSX
  , mostReadListComponent :: MostReadList.Props -> JSX
  , frontpageComponent :: Frontpage.Props -> JSX
  , user :: Maybe User
  , staticPage :: Maybe StaticPageResponse
  , categoryStructure :: Array Category
  , frontpageFeeds :: HashMap (Maybe String) (Array ArticleStub)
  }

type SetState = (State -> State) -> Effect Unit
type Props =
  { article :: Maybe FullArticle
  , mostReadArticles :: Maybe (Array ArticleStub)
  , frontpageArticles :: Maybe (Array ArticleStub)
  , staticPageContent :: Maybe StaticPage
  , categoryStructure :: Array Category
  }
type JSProps =
  { article :: Nullable Json
  , isPreview :: Nullable Boolean
  , mostReadArticles :: Nullable (Array Json)
  , frontpageArticles :: Nullable (Array Json)
  , staticPageContent :: Nullable StaticPage
  , categoryStructure :: Nullable (Array Json)
  }

app :: Component Props
app = do
  initialValues <- getInitialValues
  component "Mosaico" $ mosaicoComponent initialValues

mosaicoComponent
  :: InitialValues
  -> Props
  -> Render Unit (UseEffect Routes.MosaicoPage (UseEffect Unit (UseState State Unit))) JSX
mosaicoComponent initialValues props = React.do
  state /\ setState <- useState initialValues.state
                         { article = props.article
                         , frontpageArticles = fold props.frontpageArticles
                         , mostReadArticles = fold props.mostReadArticles
                         , staticPage = map StaticPageResponse props.staticPageContent
                         }

  useEffectOnce do
    Aff.launchAff_ do
      cats <- if null props.categoryStructure
              then Lettera.getCategoryStructure HBL
              else pure props.categoryStructure
      liftEffect do
        setState _ { categoryStructure = cats }
        -- Listen for route changes and set state accordingly
        void $ locations (routeListener cats setState) initialValues.nav
    pure mempty

  useEffect state.route do
    case state.route of
      Routes.Frontpage -> do
        if null state.frontpageArticles
        then Aff.launchAff_ do
          frontpage <- Lettera.getFrontpage HBL Nothing
          liftEffect $ setState \s -> s { frontpageArticles = frontpage
                                        , article = Nothing
                                        , frontpageFeeds = HashMap.insert Nothing frontpage s.frontpageFeeds
                                        }
        -- Set article to Nothing to prevent flickering of old article
        else setState \s -> s { article = Nothing }
      Routes.DraftPage -> pure unit
      Routes.ArticlePage _articleId -> pure unit
      Routes.MenuPage -> pure unit
      Routes.NotFoundPage _path -> pure unit
      Routes.CategoryPage category -> do
        -- TODO: Loading spinner
        case HashMap.lookup (Just category) state.frontpageFeeds of
          Just feed -> setState _ { frontpageArticles = feed }
          _ -> do
            Aff.launchAff_ do
              categoryFeed <- Lettera.getFrontpage HBL (Just category)
              liftEffect $
                setState \s -> s { frontpageArticles = categoryFeed
                                 , article = Nothing
                                 , frontpageFeeds = HashMap.insert (Just category) categoryFeed s.frontpageFeeds
                                 }
      Routes.StaticPage page
        | Just (StaticPageResponse r) <- state.staticPage
        , r.pageName == page
        -> pure unit
        | otherwise ->
          Aff.launchAff_ do
            staticPage <- fetchStaticPage page
            liftEffect $ setState _  { staticPage = Just staticPage }

    case props.mostReadArticles of
      Just mostReads
        | not $ null mostReads -> liftEffect $ setState \s -> s { mostReadArticles = mostReads }
      _ ->
        Aff.launchAff_ do
          mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
          liftEffect $ setState \s -> s { mostReadArticles = mostReadArticles }

    pure mempty

  pure $ render setState state initialValues.nav

routeListener :: Array Category -> ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener c setState _oldLoc location = do
  case match (Routes.routes c) location.pathname of
    Right path -> setState _ { route = path }
    Left _     -> pure unit

type InitialValues =
  { state :: State
  , nav :: PushStateInterface
  , locationState :: LocationState
  , initialRoute :: Routes.MosaicoPage
  }

getInitialValues :: Effect InitialValues
getInitialValues = do
  nav <- makeInterface
  locationState <- nav.locationState
  let initialRoute = either (const $ Routes.Frontpage) identity $ match (Routes.routes []) locationState.path

  articleComponent    <- Article.articleComponent
  headerComponent     <- Header.headerComponent
  menuComponent       <- Menu.menuComponent
  loginModalComponent <- LoginModal.loginModal
  mostReadListComponent <- MostReadList.mostReadListComponent
  frontpageComponent    <- Frontpage.frontpageComponent
  pure
    { state:
        { article: Nothing
        , frontpageArticles: []
        , mostReadArticles: []
        , affArticle: Nothing
        , route: initialRoute
        , clickedArticle: Nothing
        , modalView: Nothing
        , articleComponent
        , headerComponent
        , menuComponent
        , loginModalComponent
        , mostReadListComponent
        , frontpageComponent
        , user: Nothing
        , staticPage: Nothing
        , categoryStructure: []
        , frontpageFeeds: HashMap.empty
        }
    , nav
    , locationState
    , initialRoute
    }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  let isPreview = fromMaybe false $ toMaybe jsProps.isPreview
      mkFullArticle
        | isPreview = PreviewArticle
        | otherwise = FullArticle
      article = mkFullArticle <$> (hush <<< parseArticleWithoutLocalizing =<< toMaybe jsProps.article)
      mostReadArticles = map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ toMaybe jsProps.mostReadArticles
      frontpageArticles = map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ toMaybe jsProps.frontpageArticles
      staticPageContent = toMaybe jsProps.staticPageContent
      -- Decoding errors are being hushed here, although if this
      -- comes from `window.categoryStructure`, they should be
      -- valid categories
      categoryStructure = foldMap (mapMaybe (hush <<< decodeJson)) $ toMaybe jsProps.categoryStructure
  in { article, mostReadArticles, frontpageArticles, staticPageContent, categoryStructure }

jsApp :: Effect (React.ReactComponent JSProps)
jsApp = do
  initialValues <- getInitialValues
  React.reactComponent "Mosaico" $ mosaicoComponent initialValues <<< fromJSProps

render :: SetState -> State -> PushStateInterface -> JSX
render setState state router =
  case state.modalView of
    Just LoginModal ->
      state.loginModalComponent
        { onUserFetch: \user ->
           case user of
             Right u -> setState \s -> s { modalView = Nothing, user = Just u }
             Left _err ->
               -- TODO: Handle properly
               Console.error $ "Login error " <> show _err
        , onClose: setState \s -> s { modalView = Nothing }
        }
    _ -> mempty
  <> case state.route of
       Routes.CategoryPage _category ->
         mosaicoDefaultLayout $ state.frontpageComponent
           { frontpageArticles: state.frontpageArticles
           , onArticleClick: \article -> do
               setState \s -> s { clickedArticle = Just article }
               void $ Web.scroll 0 0 =<< Web.window
               router.pushState (write {}) $ "/artikel/" <> article.uuid
           }
       Routes.ArticlePage articleId
         | Just fullArticle <- state.article
         , article <- fromFullArticle fullArticle
         -- If we have this article already in `state`, let's pass that to `articleComponent`
         -- NOTE: We still need to also pass `affArticle` if there's any need to reload the article
         -- e.g. when a subscription is purchased or user logs in
         , article.uuid == articleId -> mosaicoLayoutNoAside $ renderArticle (Just fullArticle) (affArticle articleId) Nothing articleId
         | otherwise                 -> mosaicoLayoutNoAside $ renderArticle Nothing (affArticle articleId) state.clickedArticle articleId
       Routes.Frontpage -> mosaicoDefaultLayout $ state.frontpageComponent
                                { frontpageArticles: state.frontpageArticles
                                , onArticleClick: \article -> do
                                    setState \s -> s { clickedArticle = Just article }
                                    void $ Web.scroll 0 0 =<< Web.window
                                    router.pushState (write {}) $ "/artikel/" <> article.uuid
                                }

       Routes.NotFoundPage _ -> mosaicoDefaultLayout $ renderArticle (Just notFoundArticle) (pure notFoundArticle) Nothing ""
       Routes.MenuPage -> mosaicoLayoutNoAside $ state.menuComponent { visible: true }
       Routes.DraftPage -> mosaicoLayoutNoAside $ renderArticle state.article (pure notFoundArticle) Nothing $
                    fromMaybe (show UUID.emptyUUID) (_.uuid <<< fromFullArticle <$> state.article)
       Routes.StaticPage _ -> mosaicoDefaultLayout $ case state.staticPage of
         Nothing -> DOM.text "laddar"
         Just (StaticPageResponse page)  ->
           DOM.div { className: "mosaico--static-page", dangerouslySetInnerHTML: { __html: page.pageContent } }
         Just StaticPageNotFound ->
           renderArticle (Just notFoundArticle) (pure notFoundArticle) Nothing ""
         Just StaticPageOtherError -> Error.somethingWentWrong
  where
    affArticle :: String -> Aff FullArticle
    affArticle articleId = do
      a <- Lettera.getArticleAuth (fromMaybe UUID.emptyUUID $ UUID.parseUUID articleId)
      pure case a of
        Right article -> article
        Left _ -> notFoundArticle

    mosaicoDefaultLayout :: JSX -> JSX
    mosaicoDefaultLayout = flip mosaicoLayout true

    mosaicoLayoutNoAside :: JSX -> JSX
    mosaicoLayoutNoAside = flip mosaicoLayout false

    mosaicoLayout :: JSX -> Boolean -> JSX
    mosaicoLayout content showAside = DOM.div
      { className: "mosaico grid"
      , children:
          [ Header.topLine
          , state.headerComponent
              { router
              , categoryStructure: state.categoryStructure
              , onCategoryClick: \c ->
                  case state.route of
                    Routes.CategoryPage category | category == c -> pure unit
                    _ -> setState _ { frontpageArticles = [] }
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
                  [ state.mostReadListComponent
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

    renderArticle :: Maybe FullArticle -> Aff FullArticle -> Maybe ArticleStub -> String -> JSX
    renderArticle maybeA affA aStub uuid =
      state.articleComponent
        { affArticle: affA
        , brand: "hbl"
        , article: maybeA
        , articleStub: aStub
        , onLogin: setState \s -> s { modalView = Just LoginModal }
        , user: state.user
        , uuid: Just uuid
        }
