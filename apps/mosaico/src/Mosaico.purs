module Mosaico where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.UUID as UUID
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Lettera as Lettera
import Lettera.Models (Article)
import Mosaico.Article as Article
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Web.HTML (window) as Web
import Web.HTML.Location (search, setSearch) as Web
import Web.HTML.Window (location) as Web

type State =
  { article :: Maybe Article
  , articleId :: String
  , setSearch :: String -> Effect Unit
  }

app :: Component {}
app = do
  location <- Web.location =<< Web.window
  queryString <- Web.search location
  let articleId =
        case String.split (Pattern "?article=") queryString of
          ["", articleId'] -> articleId'
          _ -> mempty
  component "Mosaico" \_ -> React.do
    let initialState =
          { article: Nothing
          , articleId
          , setSearch: \newQueryString -> Web.setSearch newQueryString location
          }
    state /\ setState <- useState initialState
    useAff state.articleId $ fetchArticle setState articleId
    pure $ render state
  where
    fetchArticle setState articleId = do
      article <- Lettera.getArticle (fromMaybe UUID.emptyUUID $ UUID.parseUUID articleId)
      liftEffect $ setState \s -> s { article = Just article, articleId = articleId }

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

render :: State -> JSX
render state@{ article: Just article } =
  DOM.div
  { className: "mosaico grid"
  , children:
    [ DOM.header
      { className: "mosaico--header"
      , children: [ DOM.text "header" ]
      }
    , Article.article
      { article
      , brand: "hbl"
      }
    , DOM.footer
      { className: "mosaico--footer"
      , children: [ DOM.text "footer" ]
      }
    , DOM.aside
      { className: "mosaico--aside" }
    -- NOTE: Very crude version of article navigation (will invoke page reload though)
    -- , DOM.div
    --   { children: [ DOM.text "CLICK HERE NEW ARTICLE" ]
    --   , onClick: handler_ $ state.setSearch "?article=73220e36-2e40-4606-8e08-c8eaf753f108"
    --   }
    ]
  }
render _ = DOM.text "no article"
