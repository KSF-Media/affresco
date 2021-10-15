module Mosaico where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array (null, head)
import Data.Either (Either(..), either, hush)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
import Lettera.Models (Article, ArticleStub, FullArticle(..), notFoundArticle, fromFullArticle, parseArticleWithoutLocalizing)
import Mosaico.Article as Article
import Mosaico.Header as Header
import Mosaico.LoginModal as LoginModal
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, Render, UseEffect, UseState, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.Match (end, Match, lit, root, str)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (write)
import Web.HTML (window) as Web
import Web.HTML.Window (scroll) as Web

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | DraftPage -- Ignore parameters on client side and just show server side content
  | ArticlePage String
  | NotFoundPage String
derive instance eqR :: Eq MosaicoPage

data ModalView = LoginModal

type State =
  { article :: Maybe FullArticle
  , articleList :: Array ArticleStub
  , mostreadList :: Array ArticleStub
  , affArticle :: Maybe (Aff Article)
  , route :: MosaicoPage
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , articleComponent :: Article.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  , loginModalComponent :: LoginModal.Props -> JSX
  , user :: Maybe User
  }

type SetState = (State -> State) -> Effect Unit

type Props = { article :: Maybe FullArticle }
type JSProps = { article :: Nullable Json, isPreview :: Nullable Boolean }

routes :: Match MosaicoPage
routes = root *> oneOf
  [ DraftPage <$ (lit "artikel" *> lit "draft" *> str)
  , ArticlePage <$> (lit "artikel" *> str)
  , Frontpage <$end
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
  state /\ setState <- useState initialValues.state { article = props.article }

  -- Listen for route changes and set state accordingly
  useEffectOnce $ locations (routeListener setState) initialValues.nav
  useEffect state.route do
    case state.route of
      Frontpage -> do
        if null state.articleList
        then Aff.launchAff_ do
          frontpage <- Lettera.getFrontpage HBL
          liftEffect $ setState \s -> s { articleList = frontpage, article = Nothing }
        -- Set article to Nothing to prevent flickering of old article
        else setState \s -> s { article = Nothing }
      DraftPage -> pure unit
      ArticlePage _articleId -> pure unit
      NotFoundPage _path -> pure unit

    Aff.launchAff_ do
      mostReadArticles <- Lettera.getMostRead 0 10 "" HBL true
      liftEffect $ setState \s -> s { mostreadList = mostReadArticles }

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
  loginModalComponent <- LoginModal.loginModal
  pure
    { state:
        { article: Nothing
        , articleList: []
        , mostreadList: []
        , affArticle: Nothing
        , route: initialRoute
        , clickedArticle: Nothing
        , modalView: Nothing
        , articleComponent
        , headerComponent
        , loginModalComponent
        , user: Nothing
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
  in { article }

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
  <> DOM.div
       { className: "mosaico grid"
       , children:
           [ Header.topLine
           , state.headerComponent { router }
           , Header.mainSeparator
           , case state.route of
                 ArticlePage articleId
                   | Just fullArticle <- state.article
                   , article <- fromFullArticle fullArticle
                   -- If we have this article already in `state`, let's pass that to `articleComponent`
                   -- NOTE: We still need to also pass `affArticle` if there's any need to reload the article
                   -- e.g. when a subscription is purchased or user logs in
                   , article.uuid == articleId -> renderArticle (Just fullArticle) (affArticle articleId) Nothing articleId
                   | otherwise                 -> renderArticle Nothing (affArticle articleId) state.clickedArticle articleId
                 DraftPage -> renderArticle state.article (pure notFoundArticle) Nothing $
                              fromMaybe (show UUID.emptyUUID) (_.uuid <<< fromFullArticle <$> state.article)
                 Frontpage -> articleList state setState router
                 NotFoundPage _ -> renderArticle (Just notFoundArticle) (pure notFoundArticle) Nothing ""
           , DOM.footer
               { className: "mosaico--footer"
               , children: [ DOM.text "footer" ]
               }
           , case state.route of
               Frontpage -> 
                 DOM.aside
                    { className: "mosaico--aside"
                    , children: [ renderMostreadList state setState router ]
                    }
               _ -> mempty
           ]
       }
  where
    affArticle :: String -> Aff FullArticle
    affArticle articleId = do
      a <- Lettera.getArticleAuth (fromMaybe UUID.emptyUUID $ UUID.parseUUID articleId)
      pure case a of
        Right article -> article
        Left _ -> notFoundArticle

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

articleList :: State -> SetState -> PushStateInterface -> JSX
articleList state setState router =
  DOM.div
    { className: "mosaico--article-list"
    , children: map renderListArticle state.articleList
    }
  where
    renderListArticle :: ArticleStub -> JSX
    renderListArticle a =
      DOM.div
        { className: "mosaico--list-article list-article-default"
        , children:
            [ DOM.a
              { onClick: handler_ do
                    setState \s -> s { clickedArticle = Just a }
                    void $ Web.scroll 0 0 =<< Web.window
                    router.pushState (write {}) $ "/artikel/" <> a.uuid
              , children:
                  [ DOM.div
                    { className: "list-article-image"
                    , children:[ DOM.img { src: maybe "https://cdn.ksfmedia.fi/mosaico/hbl-fallback-img.png" _.url  a.listImage } ]
                    }
                  , DOM.div
                    { className: "list-article-liftup"
                    , children:
                        [ DOM.div
                          { className: "mosaico--tag color-hbl"
                          , children: [ DOM.text $ fromMaybe "" (head a.tags) ]
                          }
                        , DOM.h2_ [ DOM.text a.title ]
                        , DOM.div
                          { className: "mosaico--article--meta"
                          , children:
                              [ guard a.premium $
                                  DOM.div
                                    { className: "mosaico--article--premium background-hbl"
                                    , children: [ DOM.text "premium" ]
                                    }
                              ]
                          }
                        ]
                    }
                  ]
              }
            ]
        }

renderMostreadList :: State -> SetState -> PushStateInterface -> JSX
renderMostreadList state setState router =
  let
    block = "mosaico-asidelist"
  in
    DOM.div
      { className: block
      , children:
          [ DOM.h6
              { className: block <> "--header color-hbl"
              , children: [ DOM.text "Andra läser" ]
              }
          , DOM.ul
              { className: block <> "__mostread"
              , children: map renderMostreadArticle state.mostreadList
              }
          ]
      }
      where
        renderMostreadArticle :: ArticleStub -> JSX
        renderMostreadArticle a =
          DOM.li_
            [ DOM.a
                { onClick: handler_ do
                      setState \s -> s { clickedArticle = Just a }
                      void $ Web.scroll 0 0 =<< Web.window
                      router.pushState (write {}) $ "/artikel/" <> a.uuid
                , children:
                    [ DOM.div
                        { className: "counter"
                        , children: [ DOM.div { className: "background-hbl" } ]
                        }
                      , DOM.div
                          { className: "list-article-liftup"
                          , children:
                              [ DOM.h6_ [ DOM.text a.title ]
                              , DOM.div
                                  { className: "mosaico--article--meta"
                                  , children:
                                      [ guard a.premium $
                                          DOM.div
                                            { className: "mosaico--article--premium background-hbl"
                                            , children: [ DOM.text "premium" ]
                                            }
                                      ]
                                  }
                              ]
                          }
                      ]
                  }
            ]
