module Mosaico where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut.Core (Json)
import Data.Array (null, head)
import Data.Either (Either(..), either, hush)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard, mempty)
import Data.Nullable (Nullable, toMaybe)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import KSF.Paper (Paper(..))
import KSF.User (User)
import Lettera as Lettera
import Lettera.Models (Article, ArticleStub, FullArticle(..), fromFullArticle, parseArticle)
import Mosaico.Article as Article
import Mosaico.Header as Header
import Mosaico.LoginModal as LoginModal
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, Render, UseEffect, UseState, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.Match (Match, lit, root, str)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window) as Web
import Web.HTML.Window (scroll) as Web

foreign import windowArticleId :: Effect (Nullable String)

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | ArticlePage String
derive instance eqR :: Eq MosaicoPage

data ModalView = LoginModal

type State =
  { article :: Maybe FullArticle
  , articleList :: Array ArticleStub
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

type Props = { article :: Maybe Article }
type JSProps = { article :: Nullable Json }

frontpageRoute :: Match MosaicoPage
frontpageRoute = Frontpage <$ root

articleRoute :: Match MosaicoPage
articleRoute = ArticlePage <$> (lit "" *> lit "artikel" *> str)

routes :: Match MosaicoPage
routes =
  articleRoute <|> frontpageRoute

app :: Component Props
app = do
  initialValues <- getInitialValues
  component "Mosaico" $ mosaicoComponent initialValues <<< Right

-- mosaicoComponent :: InitialValues -> Either JSProps Props -> Render Unit (UseEffect State Unit) JSX
mosaicoComponent initialValues eitherProps = React.do
  state /\ setState <- useState initialValues.state

  useEffectOnce do
    props <- case eitherProps of
                  Left jsProps -> fromJSProps jsProps
                  Right props  -> pure props
    setState \s -> s { article = FullArticle <$> props.article }
    pure mempty

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
        else liftEffect $ setState \s -> s { article = Nothing }
      ArticlePage _articleId -> pure unit
    pure mempty

  pure $ render setState state initialValues.nav

routeListener :: ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener setState _oldLoc location = do
  case match routes location.pathname of
    Right path -> setState \s -> s { route = path }
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

fromJSProps :: JSProps -> Effect Props
fromJSProps jsProps = do
  article <- runMaybeT do
    a <- MaybeT $ pure $ toMaybe jsProps.article
    MaybeT $ hush <$> parseArticle a
  Console.log $ "from js props" <> fromMaybe "nope" (_.uuid <$> article)
  pure { article }

jsApp :: Effect (React.ReactComponent JSProps)
jsApp = do
  initialValues <- getInitialValues
  React.reactComponent "Mosaico" $ mosaicoComponent initialValues <<< Left

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
           , state.headerComponent { router: Just router }
           , Header.mainSeparator
           , case state.route of
                 ArticlePage articleId
                   | Just article <- fromFullArticle <$> state.article
                   , article.uuid == articleId ->
                       renderArticle state setState (pure $ FullArticle $ article { title = "ayoo" }) Nothing
                   | otherwise -> --DOM.text "YOLO"
                       let affArticle = do
                             a <- Lettera.getArticleAuth (fromMaybe UUID.emptyUUID $ UUID.parseUUID articleId)
                             case a of
                               Right article -> pure article
                               Left _ -> Aff.throwError $ error "Couldn't get article" -- TODO: handle properly
                       in renderArticle state setState affArticle state.clickedArticle
                 Frontpage -> articleList state setState router
           , DOM.footer
               { className: "mosaico--footer"
               , children: [ DOM.text "footer" ]
               }
           , DOM.aside
               { className: "mosaico--aside" }
           ]
       }

renderArticle :: State -> SetState -> Aff FullArticle -> Maybe ArticleStub -> JSX
renderArticle state setState affA aStub =
  state.articleComponent
    { affArticle: affA
    , brand: "hbl"
    , article: Nothing
    , articleStub: aStub
    , onLogin: setState \s -> s { modalView = Just LoginModal }
    , user: state.user
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
                    , children:[ DOM.img { src: fromMaybe "" $ map _.url a.listImage } ]
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
