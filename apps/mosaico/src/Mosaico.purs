module Mosaico where

import Prelude

import Routing (match)
import Control.Alt ((<|>))
import Data.Array (null)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Lettera.Models (ArticleStub, FullArticle(..))
import Mosaico.Article as Article
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing.Match (Match, lit, root, str)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (write)

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | ArticlePage String
derive instance eqR :: Eq MosaicoPage

type State =
  { article :: Maybe FullArticle
  , articleList :: Array ArticleStub
  , route :: MosaicoPage
  }

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
          , route: initialRoute
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
        ArticlePage articleId ->
          Aff.launchAff_ do
            article <- Lettera.getArticle (fromMaybe UUID.emptyUUID $ UUID.parseUUID articleId)
            liftEffect case article of
              Right a -> setState \s -> s { article = Just a }
              Left e  -> Console.log $ "errer"
      pure mempty

    pure $ render state nav

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

render :: State -> PushStateInterface -> JSX
render state router =
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
            | Just article <- state.article
            -> renderArticle article
            | otherwise
            -> articleList state router
          Frontpage -> articleList state router
    , DOM.footer
      { className: "mosaico--footer"
      , children: [ DOM.text "footer" ]
      }
    , DOM.aside
      { className: "mosaico--aside" }
    ]
  }

-- TODO: Add paywall etc.
renderArticle :: FullArticle -> JSX
renderArticle (FullArticle a)    = Article.article { article: a, brand: "hbl" }
renderArticle (PreviewArticle a) = Article.article { article: a, brand: "hbl" }

articleList :: State -> PushStateInterface -> JSX
articleList state router =
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
                { onClick: handler_ $ router.pushState (write {}) $ "/artikel/" <> a.uuid
                , children: [ DOM.text a.title ]
                }
            ]
        }
