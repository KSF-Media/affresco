module Mosaico where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Lettera as Lettera
import Lettera.Models (Article, ArticleStub)
import Mosaico.Article as Article
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Web.HTML (window) as Web
import Web.HTML.Location (search, setSearch) as Web
import Web.HTML.Window (location) as Web

type State =
  { article :: Maybe Article
  , articleId ::Maybe String
  , setSearch :: String -> Effect Unit
  , articleList :: Array ArticleStub
  }

app :: Component {}
app = do
  location <- Web.location =<< Web.window
  queryString <- Web.search location
  let articleId =
        case String.split (Pattern "?article=") queryString of
          ["", articleId'] -> Just articleId'
          _ -> Nothing
  component "Mosaico" \_ -> React.do
    let initialState =
          { article: Nothing
          , articleId
          , setSearch: \newQueryString -> Web.setSearch newQueryString location
          , articleList: []
          }
    state /\ setState <- useState initialState
    useEffectOnce $ do
      Aff.launchAff_ do
        mostReadArticles <- Lettera.getMostRead
        liftEffect $ setState \oldState -> oldState { articleList = mostReadArticles }
      pure mempty

    useAff state.articleId $ fetchArticle setState articleId
    pure $ render state
  where
    fetchArticle :: ((State -> State) -> Effect Unit) -> Maybe String -> Aff Unit
    fetchArticle setState (Just articleId) = do
      article <- Lettera.getArticle (fromMaybe UUID.emptyUUID $ UUID.parseUUID articleId)
      liftEffect $ setState \s -> s { article = Just article, articleId = Just articleId }
    fetchArticle _ Nothing = pure unit

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

render :: State -> JSX
render state =
  DOM.div
  { className: "mosaico grid"
  , children:
    [ DOM.header
      { className: "mosaico--header"
      , children: [ DOM.text "header" ]
      }
      , case state.article of
          Just article ->
            Article.article
              { article
              , brand: "hbl"
              }
          Nothing -> articleList state
    , DOM.footer
      { className: "mosaico--footer"
      , children: [ DOM.text "footer" ]
      }
    , DOM.aside
      { className: "mosaico--aside" }
    ]
  }

articleList :: State -> JSX
articleList state =
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
                -- NOTE: will invoke page reload
                { onClick: handler_ $ state.setSearch $ "?article=" <> UUID.toString a.uuid
                , children: [ DOM.text a.title ]
                }
            ]
        }
