module Mosaico where

import Prelude

import Control.Alt ((<|>))
import Data.Array (null)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Lettera.Models (ArticleStub, FullArticle(..), Article)
import Mosaico.Article as Article
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.Match (Match, lit, root, str)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (write)
import Web.HTML as Web
import Web.HTML.Window as Web

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | ArticlePage String
derive instance eqR :: Eq MosaicoPage

type State =
  { article :: Maybe FullArticle
  , articleList :: Array ArticleStub
  , affArticle :: Maybe (Aff Article)
  , route :: MosaicoPage
  , clickedArticle :: Maybe ArticleStub
  }

type SetState = (State -> State) -> Effect Unit

frontpageRoute :: Match MosaicoPage
frontpageRoute = Frontpage <$ root

articleRoute :: Match MosaicoPage
articleRoute = ArticlePage <$> (lit "" *> lit "artikel" *> str)

routes :: Match MosaicoPage
routes =
  articleRoute <|> frontpageRoute

app :: Component {}
app = do
  nav <- makeInterface
  locationState <- nav.locationState
  let initialRoute = either (const $ Frontpage) identity $ match routes locationState.path

  let routeListener :: ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
      routeListener setState _oldLoc location = do
        case match routes location.pathname of
          Right path -> setState \s -> s { route = path }
          Left err   -> pure unit

  component "Mosaico" \_ -> React.do
    let initialState =
          { article: Nothing
          , articleList: []
          , affArticle: Nothing
          , route: initialRoute
          , clickedArticle: Nothing
          }
    state /\ setState <- useState initialState

    -- Listen for route changes and set state accordingly
    useEffectOnce $ locations (routeListener setState) nav
    useEffect state.route do
      case state.route of
        Frontpage -> do
          if null state.articleList
          then Aff.launchAff_ do
            frontPage <- Lettera.getFrontpage HBL
            case frontPage of
              Right fp -> liftEffect $ setState \s -> s { articleList = fp, article = Nothing }
              Left err -> Aff.throwError $ error err
          -- Set article to Nothing to prevent flickering of old article
          else liftEffect $ setState \s -> s { article = Nothing }
        ArticlePage articleId -> do
          let a = Lettera.getArticle' (fromMaybe UUID.emptyUUID $ UUID.parseUUID articleId)
          setState \s -> s { affArticle = Just a }
      pure mempty

    pure $ render setState state nav

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

render :: SetState -> State -> PushStateInterface -> JSX
render setState state router =
  DOM.div
  { className: "mosaico grid"
  , children:
    [ DOM.header
      { className: "mosaico--header"
      , children: [ DOM.text "header" ]
      , onClick: handler_ $ router.pushState (write {}) "/"
      }
      , case state.route of
          ArticlePage _
            | Just affArticle <- state.affArticle
            -> renderArticle affArticle state.clickedArticle
            | otherwise
            -> articleList state setState router
          Frontpage -> articleList state setState router
    , DOM.footer
      { className: "mosaico--footer"
      , children: [ DOM.text "footer" ]
      }
    , DOM.aside
      { className: "mosaico--aside" }
    ]
  }

renderArticle affA aStub = Article.article {affArticle: affA, brand: "hbl", articleStub: aStub}

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
        { className: "mosaico--list-article"
        , children:
            [ DOM.div
                { onClick: handler_ do
                     setState \s -> s { clickedArticle = Just a }
                     window <- Web.window
                     _ <- Web.scroll 0 0 window
                     router.pushState (write {}) $ "/artikel/" <> a.uuid
                , children:
                    [ DOM.img { src: fromMaybe "" $ map _.url a.listImage }
                      , DOM.text a.title ]
                }
            ]
        }
