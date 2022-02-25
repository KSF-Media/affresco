module Mosaico.LatestList where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import Data.String (joinWith)
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
           [ DOM.h3
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
                        [ DOM.h6_ [ DOM.text $ fromMaybe a.title a.listTitle ]
                        , guard a.premium $ DOM.div
                            { className: "mosaico--article--meta"
                            , children:
                                [ DOM.div
                                    { className: "premium-badge"
                                    , children: [ DOM.text "premium" ]
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
