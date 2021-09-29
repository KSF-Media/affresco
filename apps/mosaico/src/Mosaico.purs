module Mosaico where

import Prelude

import Affjax (get) as AX
import Affjax.ResponseFormat (string) as AX
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json)
import Data.Array (mapMaybe, null)
import Data.Either (Either(..), either, hush)
import Data.Foldable (fold, oneOf)
import Data.Maybe (Maybe(..), fromMaybe)
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
import Lettera.Models (Article, ArticleStub, FullArticle(..), fromFullArticle, notFoundArticle, parseArticleStubWithoutLocalizing, parseArticleWithoutLocalizing)
import Mosaico.Article as Article
import Mosaico.Frontpage as Frontpage
import Mosaico.Header as Header
import Mosaico.Header.Menu as Menu
import Mosaico.LoginModal as LoginModal
import Mosaico.MostReadList as MostReadList
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, Render, UseEffect, UseState, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (write)
import Web.HTML (window) as Web
import Web.HTML.Window (scroll) as Web

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | DraftPage -- Ignore parameters on client side and just show server side content
  | ArticlePage String
  | NotFoundPage String
  | StaticPage String
  | MenuPage
derive instance eqR :: Eq MosaicoPage

data ModalView = LoginModal

type State =
  { article :: Maybe FullArticle
  , frontpageArticles :: Array ArticleStub
  , mostReadArticles :: Array ArticleStub
  , affArticle :: Maybe (Aff Article)
  , route :: MosaicoPage
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , articleComponent :: Article.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  , menuComponent :: Menu.Props -> JSX
  , loginModalComponent :: LoginModal.Props -> JSX
  , mostReadListComponent :: MostReadList.Props -> JSX
  , frontpageComponent :: Frontpage.Props -> JSX
  , user :: Maybe User
  , staticPage :: Maybe (Either StaticPageError JSX)
  }

type SetState = (State -> State) -> Effect Unit

data StaticPageError
  = StaticPageNotFound
  | StaticPageOtherError

type Props =
  { article :: Maybe FullArticle
  , mostReadArticles :: Maybe (Array ArticleStub)
  , frontpageArticles :: Maybe (Array ArticleStub)
  }
type JSProps =
  { article :: Nullable Json
  , isPreview :: Nullable Boolean
  , mostReadArticles :: Nullable (Array Json)
  , frontpageArticles :: Nullable (Array Json)
  }

routes :: Match MosaicoPage
routes = root *> oneOf
  [ DraftPage <$ (lit "artikel" *> lit "draft" *> str)
  , ArticlePage <$> (lit "artikel" *> str)
  , StaticPage <$> (lit "sida" *> str)
  , Frontpage <$end
  , MenuPage <$ lit "meny"
  , NotFoundPage <$> str
  ]

app :: Component Props
app = do
  initialValues <- getInitialValues
  component "Mosaico" $ mosaicoComponent initialValues

mosaicoComponent
  :: InitialValues
  -> Props
  -> Render Unit (UseEffect MosaicoPage (UseEffect Unit (UseState State Unit))) JSX
mosaicoComponent initialValues props = React.do
  state /\ setState <- useState initialValues.state
                         { article = props.article
                         , frontpageArticles = fold props.frontpageArticles
                         , mostReadArticles = fold props.mostReadArticles
                         }

  -- Listen for route changes and set state accordingly
  useEffectOnce $ locations (routeListener setState) initialValues.nav
  useEffect state.route do
    case state.route of
      Frontpage -> do
        if null state.frontpageArticles
        then Aff.launchAff_ do
          frontpage <- Lettera.getFrontpage HBL
          liftEffect $ setState \s -> s { frontpageArticles = frontpage, article = Nothing }
        -- Set article to Nothing to prevent flickering of old article
        else setState \s -> s { article = Nothing }
      DraftPage -> pure unit
      ArticlePage _articleId -> pure unit
      MenuPage -> pure unit
      NotFoundPage _path -> pure unit
      StaticPage page -> Aff.launchAff_ do
        let staticPageUrl = "https://cdn.ksfmedia.fi/mosaico/static/" <> page <> ".html"
        res <- AX.get AX.string staticPageUrl
        liftEffect $ case res of
          Right content ->
            setState _ { staticPage = Just $ case content.status of
              StatusCode 200 -> Right $ DOM.div { className: "mosaico--static-page", dangerouslySetInnerHTML: { __html: content.body } }
              StatusCode 404 -> Left StaticPageNotFound
              _ -> Left StaticPageOtherError
            }
          Left _err -> setState _ { staticPage = Just $ Left StaticPageOtherError }


    case props.mostReadArticles of
      Just mostReads
        | not $ null mostReads -> liftEffect $ setState \s -> s { mostReadArticles = mostReads }
      _ ->
        Aff.launchAff_ do
          mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
          liftEffect $ setState \s -> s { mostReadArticles = mostReadArticles }

    pure mempty

  pure $ render setState state initialValues.nav

routeListener :: ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener setState _oldLoc location = do
  case match routes location.pathname of
    Right path -> setState _ { route = path }
    Left _     -> pure unit

type InitialValues =
  { state :: State
  , nav :: PushStateInterface
  , locationState :: LocationState
  , initialRoute :: MosaicoPage
  }

getInitialValues :: Effect InitialValues
getInitialValues = do
  nav <- makeInterface
  locationState <- nav.locationState
  let initialRoute = either (const $ Frontpage) identity $ match routes locationState.path

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
  in { article, mostReadArticles, frontpageArticles }

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
       ArticlePage articleId
         | Just fullArticle <- state.article
         , article <- fromFullArticle fullArticle
         -- If we have this article already in `state`, let's pass that to `articleComponent`
         -- NOTE: We still need to also pass `affArticle` if there's any need to reload the article
         -- e.g. when a subscription is purchased or user logs in
         , article.uuid == articleId -> mosaicoLayoutNoAside $ renderArticle (Just fullArticle) (affArticle articleId) Nothing articleId
         | otherwise                 -> mosaicoLayoutNoAside $ renderArticle Nothing (affArticle articleId) state.clickedArticle articleId
       Frontpage -> state.frontpageComponent
                                { frontpageArticles: state.frontpageArticles
                                , onArticleClick: \article -> do
                                    setState \s -> s { clickedArticle = Just article }
                                    void $ Web.scroll 0 0 =<< Web.window
                                    router.pushState (write {}) $ "/artikel/" <> article.uuid
                                }
       NotFoundPage _ -> mosaicoDefaultLayout $ renderArticle (Just notFoundArticle) (pure notFoundArticle) Nothing ""
       MenuPage -> mosaicoLayoutNoAside $ state.menuComponent { visible: true }
       DraftPage -> renderArticle state.article (pure notFoundArticle) Nothing $
                    fromMaybe (show UUID.emptyUUID) (_.uuid <<< fromFullArticle <$> state.article)
       StaticPage _ -> mosaicoDefaultLayout $ case state.staticPage of
         Nothing -> DOM.text "laddar"
         Just (Right content) -> content
         Just (Left StaticPageNotFound) -> renderArticle (Just notFoundArticle) (pure notFoundArticle) Nothing ""
         Just (Left StaticPageOtherError) ->
           DOM.div
             { className: "mosaico--static-page-error"
             , children: [ DOM.text "Oj! Något gick fel, ladda om sidan." ]
             }
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
          , state.headerComponent { router }
          , Header.mainSeparator
          , content
          , DOM.footer
              { className: "mosaico--footer"
              , children: [ DOM.text "footer" ]
              }
          , guard showAside $ DOM.aside
              { className: "mosaico--aside"
              , children: [ renderMostreadList state setState router ]
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
