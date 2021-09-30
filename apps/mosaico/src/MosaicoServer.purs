module MosaicoServer where

import Prelude

import Mosaico.Article as Article
import Mosaico.Header as Header
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, useState, (/\))
import React.Basic.Hooks as React
import Routing.PushState (makeInterface, PushStateInterface )

type Props =
  { mainContent :: JSX }

type State =
  { articleComponent :: Article.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  }

app :: Component Props
app = do
  articleComponent <- Article.articleComponent
  headerComponent  <- Header.headerComponent
  emptyRouter <- makeInterface
  component "Mosaico" \props -> React.do
    let initialState =
          { articleComponent
          , headerComponent
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
               , children: [ DOM.text "footer" ]
               }
           , DOM.aside
               { className: "mosaico--aside" }
           ]
       }
