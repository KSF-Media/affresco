module Mosaico.MostReadList where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import Data.String (joinWith)
import Lettera.Models (ArticleStub)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)


type Props =
  { mostReadArticles :: Array ArticleStub
  , onClickHandler :: ArticleStub -> EventHandler
  }

render :: Props -> JSX
render props =
  let block =  "mosaico-asidelist"
  in DOM.div
       { className: joinWith " " [block, block <> "__mostread"]
       , children:
           [ DOM.h3
               { className: block <> "--header"
               , children: [ DOM.text "Andra läser" ]
               }
           , DOM.ul_ $ map renderMostreadArticle props.mostReadArticles
           ]
       }
  where
    renderMostreadArticle :: ArticleStub -> JSX
    renderMostreadArticle a =
      DOM.li_
        [ DOM.a
            { onClick: props.onClickHandler a
            , href: "/artikel/" <> a.uuid
            , children:
                [ DOM.div
                    { className: "counter"
                    , children: [ DOM.div_ [] ]
                    }
                , DOM.div
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
