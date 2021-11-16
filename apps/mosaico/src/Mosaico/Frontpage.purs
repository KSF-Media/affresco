module Mosaico.Frontpage where

import Prelude

import Data.Array (head)
import Data.Foldable (foldMap)
import Data.Maybe (maybe)
import Data.Monoid (guard)
import Data.Newtype (un)
import Effect (Effect)
import Lettera.Models (ArticleStub, Tag(..), tagToURIComponent)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component)

type Props =
  { frontpageArticles :: Array ArticleStub
  , onArticleClick :: ArticleStub -> Effect Unit
  , onTagClick :: Tag -> Effect Unit
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
        , children:
            [ DOM.a
              { onClick: handler_ $ props.onArticleClick a
              , children:
                  [ DOM.div
                    { className: "list-article-image"
                    , children:[ DOM.img { src: maybe "https://cdn.ksfmedia.fi/mosaico/hbl-fallback-img.png" _.url  a.listImage } ]
                    }
                  , DOM.div
                    { className: "list-article-liftup"
                    , children:
                        [ DOM.div
                          { className: "mosaico-article__tag color-hbl"
                          , onClick: handler preventDefault $ const $ foldMap props.onTagClick $ head a.tags
                          , children: [ DOM.text $ foldMap (un Tag) (head a.tags) ]
                          }
                        , DOM.h2_ [ DOM.text a.title ]
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
