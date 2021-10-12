module Mosaico.MostReadList where

import Prelude

import Data.Monoid (guard)
import Effect (Effect)
import Lettera.Models (ArticleStub)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component)

type Props =
  { mostReadArticles :: Array ArticleStub
  , onClickHandler :: ArticleStub -> Effect Unit
  }

mostReadListComponent :: Component Props
mostReadListComponent = do
  component "MostReadListComponent" \props -> React.do
    pure $ render props

render :: Props -> JSX
render props =
  let block =  "mosaico-asidelist"
  in DOM.div
       { className: block
       , children:
           [ DOM.h6
               { className: block <> "--header color-hbl"
               , children: [ DOM.text "Andra läser" ]
               }
           , DOM.ul
               { className: block <> "__mostread"
               , children: map renderMostreadArticle props.mostReadArticles
               }
           ]
       }
  where
    renderMostreadArticle :: ArticleStub -> JSX
    renderMostreadArticle a =
      DOM.li_
        [ DOM.a
            { onClick: handler preventDefault $ const $ props.onClickHandler a
            , href: "/artikel/" <> a.uuid
            , children:
                [ DOM.div
                    { className: "counter"
                    , children: [ DOM.div { className: "background-hbl" } ]
                    }
                , DOM.div
                    { className: "list-article-liftup"
                    , children:
                        [ DOM.h6_ [ DOM.text a.title ]
                        , DOM.div
                            { className: "mosaico--article--meta"
                            , children:
                                [ guard a.premium $
                                    DOM.div
                                      { className: "mosaico--article--premium background-hbl"
                                      , children: [ DOM.text "premium" ]
                                      }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
