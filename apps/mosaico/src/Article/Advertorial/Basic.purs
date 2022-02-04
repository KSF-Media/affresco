module Mosaico.Article.Advertorial.Basic
  ( Props
  , render
  )
  where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Lettera.Models (FullArticle)
import React.Basic (JSX)
import React.Basic.DOM as DOM

getTitle :: FullArticle -> String
getTitle fullArticle = _.title $ fromFullArticle fullArticle

getPreamble :: FullArticle -> Maybe String
getPreamble fullArticle = _.preamble $ fromFullArticle fullArticle

type Props =
  { article :: FullArticle
  , paper :: Paper
  }
render :: Props -> JSX
render props =
  let title = getTitle props.article
  in DOM.article
    { children:
      [ DOM.div
        { className: "advertorial__top-bar"
        , children:
          [ DOM.span_ [ DOM.text "Annos: Company name"]
          ]
        }
      , DOM.h1
        { className: "advertorial__headline"
        , children: [ DOM.text title ]
        }
      , DOM.section
        { className: "advertorial__preamble"
        , children: [ DOM.text $ fromMaybe mempty $ getPreamble props.article ]
        }
      ]
    }
