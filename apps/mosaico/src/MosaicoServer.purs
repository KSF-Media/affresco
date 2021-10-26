module MosaicoServer where

import Prelude

import Lettera.Models (ArticleStub)
import Mosaico.Article as Article
import Mosaico.Header as Header
import Mosaico.MostReadList as MostReadList
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, useState, (/\))
import React.Basic.Hooks as React
import Routing.PushState (PushStateInterface)
import Simple.JSON (write)

type Props =
  { mainContent :: JSX
  , mostReadArticles :: Array ArticleStub
  }

type State =
  { articleComponent :: Article.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  , mostReadListComponent :: MostReadList.Props -> JSX
  }

app :: Component Props
app = do
  articleComponent <- Article.articleComponent
  headerComponent  <- Header.headerComponent
  mostReadListComponent <- MostReadList.mostReadListComponent
  let (emptyRouter :: PushStateInterface) =
        { listen: const $ pure $ pure unit
        , locationState:
            pure
              { hash: mempty
              , path: mempty
              , pathname: mempty
              , search: mempty
              , state: write {}
              }
        , pushState: const $ const mempty
        , replaceState: const $ const mempty
        }
  component "Mosaico" \props -> React.do
    let initialState =
          { articleComponent
          , headerComponent
          , mostReadListComponent
          }
    state /\ _setState <- useState initialState
    pure $ render emptyRouter state props


render :: PushStateInterface -> State -> Props -> JSX
render router state props = DOM.div
       { className: "mosaico grid"
       , children:
           [ Header.topLine
           , state.headerComponent { router }
           , Header.mainSeparator
           , props.mainContent
           , DOM.footer
               { className: "mosaico--footer"
               , children: 
                  [ DOM.text "footer" ]
               }
           , DOM.aside
               { className: "mosaico--aside"
               , children:
                  [ state.mostReadListComponent
                      { mostReadArticles: props.mostReadArticles
                      , onClickHandler: const $ pure unit
                      }
                  ]
               }
           ]
       }
