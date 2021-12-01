module Mosaico.Frontpage where

import Prelude

import Data.Array (head)
import Data.Foldable (foldMap)
import Data.Maybe (maybe)
import Data.Monoid (guard)
import Data.Newtype (un)
import Lettera.Models (ArticleStub, Tag(..), tagToURIComponent)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component, component)

type Props =
  { frontpageArticles :: Array ArticleStub
  , onArticleClick :: ArticleStub -> EventHandler
  , onTagClick :: Tag -> EventHandler
  }

frontpageComponent :: Component Props
frontpageComponent = component "FrontpageComponent" $ pure <<< render

render :: Props -> JSX
render props =
   DOM.div
    { className: "mosaico--article-list"
    , children: map renderListArticle props.frontpageArticles
    }
  where
    renderListArticle :: ArticleStub -> JSX
    renderListArticle a =
      DOM.div
        { className: "mosaico--list-article list-article-default"
        , onClick: props.onArticleClick a
        , children:
            [ DOM.span
                { children:
                    [ DOM.a
                        { href: "/artikel/" <> a.uuid
                        , className: "list-article-image"
                        , children: [ DOM.img { src: maybe "https://cdn.ksfmedia.fi/mosaico/hbl-fallback-img.png" _.url  a.listImage } ]
                        }
                    ,  DOM.div
                         { className: "list-article-liftup"
                         , children:
                             [ foldMap
                                 (\tag ->
                                     DOM.a
                                       { className: "mosaico-article__tag color-hbl"
                                       , onClick: props.onTagClick tag
                                       , href: "/tagg/" <> tagToURIComponent tag
                                       , children: [ DOM.text $ un Tag tag ]
                                       }
                                 ) $ head a.tags
                             , DOM.a
                                 { href: "/artikel/" <> a.uuid
                                 , children: [ DOM.h2_ [ DOM.text a.title ] ]
                                 }
                             , guard a.premium $
                               DOM.div
                                 { className: "mosaico--article--meta"
                                 , children:
                                     [ DOM.div
                                         { className: "premium-badge background-hbl"
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
