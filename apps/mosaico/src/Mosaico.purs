module Mosaico where

import Prelude

import Control.Alternative as Alt
import Data.Argonaut.Core (Json, toArray, fromString, isArray,jsonEmptyArray)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapMaybe, null, head, length)
import Data.Either (Either(..), either, hush, isRight, fromRight)
import Data.Foldable (fold, foldMap)
import Data.Hashable (class Hashable, hash)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Nullable (Nullable, toMaybe)
import Data.String as String
import Data.UUID as UUID
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign as Foreign
import KSF.Paper (Paper(..))
import KSF.User (User)
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category, CategoryLabel (..), FullArticle(..), Tag (..), isPreviewArticle, fromFullArticle, notFoundArticle, parseArticleStubWithoutLocalizing, parseArticleWithoutLocalizing, tagToURIComponent, uriComponentToTag)
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
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, Render, UseEffect, UseState, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (write)
import Web.HTML (window) as Web
import Web.HTML.Window (scroll) as Web
import Unsafe.Coerce (unsafeCoerce)

data ModalView = LoginModal

type State =
  { article :: Maybe FullArticle
  , frontpageArticles :: Array ArticleStub
  , mostReadArticles :: Array ArticleStub
  -- , tagArticlesName :: Maybe Tag
  -- , tagArticlesLoading :: Boolean
  -- , tagArticles :: Array ArticleStub
  , route :: Routes.MosaicoPage
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , headerComponent :: Header.Props -> JSX
  , menuComponent :: Menu.Props -> JSX
  , loginModalComponent :: LoginModal.Props -> JSX
  , mostReadListComponent :: MostReadList.Props -> JSX
  , frontpageComponent :: Frontpage.Props -> JSX
  , user :: Maybe User
  , staticPage :: Maybe StaticPageResponse
  , categoryStructure :: Array Category
  , frontpageFeeds :: HashMap ArticleFeed (Array ArticleStub) -- HashMap (Maybe CategoryLabel) (Array ArticleStub)
  }

type SetState = (State -> State) -> Effect Unit
type Props =
  { article :: Maybe FullArticle
  , mostReadArticles :: Maybe (Array ArticleStub)
  -- , frontpageArticles :: Maybe (Array ArticleStub)
  , staticPageContent :: Maybe StaticPage
  , categoryStructure :: Array Category
  , tagArticlesName :: Maybe Tag
  , tagArticles :: Maybe (Array ArticleStub)
  , initialFrontpageFeed :: HashMap ArticleFeed (Array ArticleStub)
  }
type JSProps =
  { article :: Nullable Json
  , isPreview :: Nullable Boolean
  , mostReadArticles :: Nullable (Array Json)
  -- , frontpageArticles :: Nullable (Array Json)
  , staticPageContent :: Nullable StaticPage
  , categoryStructure :: Nullable (Array Json)
  , tagArticlesName :: Nullable String
  , tagArticles :: Nullable (Array Json)
  , initialFrontpageFeed :: Nullable { feedType    :: Nullable String
                              , feedPage    :: Nullable String
                              , feedContent :: Nullable String
                              }
  }

data ArticleFeed
  = CategoryFeed (Maybe CategoryLabel) -- `Nothing` represents root
  | TagFeed Tag
derive instance eqArticleFeed :: Eq ArticleFeed
instance showArticleFeed :: Show ArticleFeed where
  show (CategoryFeed c) = "CategoryFeed " <> show c
  show (TagFeed t) = "TagFeed" <> show t
instance hashableArticleFeed :: Hashable ArticleFeed where
  hash = hash <<< show

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
                        -- , frontpageArticles = fold props.frontpageArticles
                         , mostReadArticles = fold props.mostReadArticles
                         , staticPage = map StaticPageResponse props.staticPageContent
                         -- , tagArticlesName = props.tagArticlesName
                         -- , tagArticlesLoading = false
                         -- , tagArticles = fold props.tagArticles
                         , categoryStructure = props.categoryStructure
                         , frontpageFeeds = props.initialFrontpageFeed
                    --     , mainArticleFeed = fold props.mainArticleFeed
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
    -- Console.log $ "wat " <> (show $ HashMap.keys props.frontpageFeed)
    -- Console.log $ "FEED z " <> (show $ length $ fromMaybe (unsafeCoerce ["aaa", "a", "a"]) $ HashMap.lookup (CategoryFeed Nothing) props.frontpageFeed)

    -- Console.log $ "FEED 1 " <> (maybe "nope" (_.title) (head =<< HashMap.lookup (CategoryFeed Nothing) props.frontpageFeed))
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
      Routes.Frontpage
        -- Do we already have the front page feed?
        | Just f <- HashMap.lookup (CategoryFeed Nothing) state.frontpageFeeds -> pure unit
        -- If no, fetch from Lettera
        | otherwise -> do
          Aff.launchAff_ do
            frontpage <- Lettera.getFrontpage HBL Nothing
            liftEffect $ setState \s -> s { frontpageArticles = frontpage
                                          , article = Nothing
                                          , frontpageFeeds = HashMap.insert (CategoryFeed Nothing) frontpage s.frontpageFeeds
                                          }
      Routes.TagPage tag ->
        case HashMap.lookup (TagFeed tag) state.frontpageFeeds of
          Just feed -> setState _ { frontpageArticles = feed }
          Nothing   -> Aff.launchAff_ do
            tagFeed <- Lettera.getByTag 0 20 tag HBL
            liftEffect $ setState \s -> s { frontpageArticles = tagFeed
                                          , frontpageFeeds = HashMap.insert (TagFeed tag) tagFeed s.frontpageFeeds
                                          }
        -- | Just tag == state.tagArticlesName -> pure unit
        -- | otherwise -> do
        --     setState _ { tagArticlesName = Just tag
        --                , tagArticles = mempty
        --                , tagArticlesLoading = true
        --                }
        --     Aff.launchAff_ do
        --       byTag <- Lettera.getByTag 0 20 tag HBL
        --       liftEffect $ setState _ { tagArticlesName = Just tag
        --                               , tagArticles = byTag
        --                               , tagArticlesLoading = false
        --                               }
      -- Always uses server side provided article
      Routes.DraftPage -> pure unit
      Routes.ArticlePage articleId
        | (isPreviewArticle <$> state.article) == Just true -> loadArticle articleId
        | Just articleId == ((_.uuid <<< fromFullArticle) <$> state.article) -> pure unit
        | otherwise -> loadArticle articleId
      Routes.MenuPage -> pure unit
      Routes.NotFoundPage _path -> pure unit
      Routes.CategoryPage category -> do
        -- TODO: Loading spinner
        case HashMap.lookup (CategoryFeed (Just category)) state.frontpageFeeds of
          Just feed -> setState _ { frontpageArticles = feed }
          _ -> do
            Aff.launchAff_ do
              categoryFeed <- Lettera.getFrontpage HBL (Just $ show category)
              liftEffect $
                setState \s -> s { frontpageArticles = categoryFeed
                                 , article = Nothing
                                 , frontpageFeeds = HashMap.insert (CategoryFeed (Just category)) categoryFeed s.frontpageFeeds
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

  pure $ render setState state initialValues.nav $ maybe (pure unit) loadArticle $ _.uuid <<< fromFullArticle <$> state.article

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
        -- , tagArticlesName: Nothing
        -- , tagArticlesLoading: false
        -- , tagArticles: []
        , route: initialRoute
        , clickedArticle: Nothing
        , modalView: Nothing
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

      frontpageFeed :: Maybe (HashMap ArticleFeed (Array ArticleStub))
      frontpageFeed = do --map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ toMaybe jsProps.frontpageFeed
        feed <- toMaybe jsProps.initialFrontpageFeed
        let feedPage = toMaybe feed.feedPage
        feedType <- do
          f <- toMaybe feed.feedType
          case String.toLower f of
            "categoryfeed" -> Just $ CategoryFeed (map CategoryLabel feedPage)
            "tagfeed" -> map (TagFeed <<< Tag) feedPage
            _ -> Nothing
        let feedContent = fold do
          --    _ <- Debug.trace ("aaa1") (\_ -> Just "")
              f <- toMaybe feed.feedContent

              let j = jsonParser f
              _ <- Debug.trace ("jsoni " <> (show (either (const $ false) (isArray) j))) (\_ -> Just "")
              jj <- either (\_ -> Just [jsonEmptyArray]) toArray j
              -- a <- toArray j
              _ <- Debug.trace ("aaa3" <> unsafeCoerce jj) (\_ -> Just "")
              pure $ mapMaybe (hush <<< parseArticleStubWithoutLocalizing) jj
        -- Alt.guard (not $ null feedContent)
    --    ff <- toMaybe feed.feedContent
      --  _ <- Debug.trace ("asd" <> (unsafeCoerce $ Foreign.readArray ff)) (\_ -> Just "")
        pure $ HashMap.singleton feedType feedContent

      staticPageContent = toMaybe jsProps.staticPageContent
      -- Decoding errors are being hushed here, although if this
      -- comes from `window.categoryStructure`, they should be
      -- valid categories
      categoryStructure = foldMap (mapMaybe (hush <<< decodeJson)) $ toMaybe jsProps.categoryStructure
      tagArticlesName = uriComponentToTag <$> toMaybe jsProps.tagArticlesName
      tagArticles = mapMaybe (hush <<< parseArticleStubWithoutLocalizing) <$> toMaybe jsProps.tagArticles
  in { article, mostReadArticles, initialFrontpageFeed: fromMaybe HashMap.empty frontpageFeed, staticPageContent, categoryStructure, tagArticles, tagArticlesName }

jsApp :: Effect (React.ReactComponent JSProps)
jsApp = do
  initialValues <- getInitialValues
  React.reactComponent "Mosaico" $ mosaicoComponent initialValues <<< fromJSProps

render :: SetState -> State -> PushStateInterface -> Effect Unit -> JSX
render setState state router onPaywallEvent =
  case state.modalView of
    Just LoginModal ->
      state.loginModalComponent
        { onUserFetch: \user ->
           case user of
             Right u -> do
               setState _ { modalView = Nothing, user = Just u }
               onPaywallEvent
             Left _err ->
               -- TODO: Handle properly
               Console.error $ "Login error " <> show _err
        , onClose: setState \s -> s { modalView = Nothing }
        }
    _ -> mempty
  <> case state.route of
       Routes.CategoryPage category ->
         mosaicoDefaultLayout $ state.frontpageComponent
           { frontpageArticles: fold $ HashMap.lookup (CategoryFeed (Just category)) state.frontpageFeeds
           , onArticleClick: \article -> do
               setState _ { clickedArticle = Just article }
               void $ Web.scroll 0 0 =<< Web.window
               router.pushState (write {}) $ "/artikel/" <> article.uuid
           , onTagClick
           }
       Routes.ArticlePage articleId
         | Just fullArticle <- state.article
         , article <- fromFullArticle fullArticle
         -- If we have this article already in `state`, let's pass that to `articleComponent`
         , article.uuid == articleId -> mosaicoLayoutNoAside $ renderArticle (Right fullArticle)
         | Just stub <- state.clickedArticle -> mosaicoLayoutNoAside $ renderArticle $ Left stub
         | otherwise -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.Frontpage -> frontpage $ fold $ HashMap.lookup (CategoryFeed Nothing) state.frontpageFeeds
       Routes.NotFoundPage _ -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.TagPage tag
         | Just tagFeed <- HashMap.lookup (TagFeed tag) state.frontpageFeeds
         -> frontpage tagFeed
         | otherwise -> renderArticle (Right notFoundArticle)
         -- if state.tagArticlesLoading || (not $ null state.tagArticles)
         --   then frontpage state.tagArticles
         --   else renderArticle (Right notFoundArticle)
       Routes.MenuPage ->
         mosaicoLayoutNoAside
         $ state.menuComponent
             { categoryStructure: state.categoryStructure
             , onCategoryClick: \categoryLabel url -> do
                 onCategoryClick categoryLabel
                 router.pushState (write {}) url
             }
       Routes.DraftPage -> mosaicoLayoutNoAside
         $ renderArticle $ maybe (Right notFoundArticle) Right state.article
       Routes.StaticPage _ -> mosaicoDefaultLayout $ case state.staticPage of
         Nothing -> DOM.text "laddar"
         Just (StaticPageResponse page)  ->
           DOM.div { className: "mosaico--static-page", dangerouslySetInnerHTML: { __html: page.pageContent } }
         Just StaticPageNotFound ->
           renderArticle (Right notFoundArticle)
         Just StaticPageOtherError -> Error.somethingWentWrong
  where
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
              , onCategoryClick
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

    onCategoryClick c =
      case state.route of
        Routes.CategoryPage category | category == c -> pure unit
        _ -> setState _ { frontpageArticles = [] }

    onTagClick tag = capture_ do
      void $ Web.scroll 0 0 =<< Web.window
      router.pushState (write {}) $ "/tagg/" <> tagToURIComponent tag

    frontpage frontpageArticles = mosaicoDefaultLayout $ state.frontpageComponent
      { frontpageArticles
      , onArticleClick: \article -> do
          setState \s -> s { clickedArticle = Just article }
          void $ Web.scroll 0 0 =<< Web.window
          router.pushState (write {}) $ "/artikel/" <> article.uuid
      , onTagClick
      }

    renderArticle :: Either ArticleStub FullArticle -> JSX
    renderArticle article =
      Article.render
        { brand: "hbl"
        , article
        , onLogin: setState \s -> s { modalView = Just LoginModal }
        , onPaywallEvent
        , onTagClick
        }
