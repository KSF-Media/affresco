module Mosaico.LatestList where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import KSF.Helpers (formatArticleTime)
import Lettera.Models (ArticleStub)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

type Props =
  { latestArticles :: Array ArticleStub
  , onClickHandler :: ArticleStub -> EventHandler
  }

render :: Props -> JSX
render props =
  let block =  "mosaico-asidelist"
  in DOM.div
       { className: joinWith " " [block, block <> "__latest"]
       , children:
           [ DOM.h2
               { className: block <> "--header"
               , children: [ DOM.text "Senast publicerat" ]
               }
           , DOM.ul_ $ map renderLatestArticle props.latestArticles
           ]
       }
  where
    renderLatestArticle :: ArticleStub -> JSX
    renderLatestArticle a =
      DOM.li_
        [ DOM.a
            { onClick: props.onClickHandler a
            , href: "/artikel/" <> a.uuid
            , children:
                [ DOM.div
                    { className: "list-article-liftup"
                    , children:
                        [ DOM.h3
                            { className: "text-xl leading-tight font-duplexserif"
                            , children: [ DOM.text $ fromMaybe a.title a.listTitle ]
                            }
                        , foldMap (\publishingTime ->
                            DOM.span
                              { className: "mosaico-asidelist__timestamp"
                              , children:
                                  [ DOM.span_ [ DOM.text $ formatArticleTime $ unwrap publishingTime ] ]
                              }
                          ) $ a.publishingTime
                        , guard a.premium $ DOM.div
                            { className: "mosaico-article__meta"
                            , children:
                                [ DOM.div
                                    { className: "premium-badge"
                                    , children: [ DOM.span_ [ DOM.text "premium" ]]
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
