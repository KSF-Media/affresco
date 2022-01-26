module Mosaico.Article.Advertorial.Basic
  ( Props
  , render
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import KSF.Paper (Paper)
import Lettera.Models (FullArticle)
import Mosaico.Article as Article
import Mosaico.Article.Image as Image
import React.Basic (JSX)
import React.Basic.DOM as DOM

type Props =
  { article :: FullArticle
  , paper :: Paper
  }
render :: Props -> JSX
render props =
  DOM.div
    { className: "announcement-header"
    , children:
      [ DOM.text "Annons" ]
    } <> Article.render (Image.render mempty)
    { paper: props.paper
    , article: Right props.article
    , onLogin: mempty
    , onPaywallEvent: pure unit
    , onTagClick: const mempty
    , onArticleClick: const mempty
    , user: Nothing
    , mostReadArticles: mempty
    , latestArticles: mempty
    }

