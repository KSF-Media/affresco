module MosaicoServer where

import Prelude

import Data.Maybe (Maybe(..))
import Mosaico.Article as Article
import Mosaico.Header as Header
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, useState, (/\))
import React.Basic.Hooks as React

type Props =
  { mainContent :: JSX }

type State =
  { articleComponent :: Article.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  }

app :: Component Props
app = do
  articleComponent    <- Article.articleComponent
  headerComponent     <- Header.headerComponent
  component "Mosaico" \props -> React.do
    let initialState =
          { articleComponent
          , headerComponent
          }
    state /\ setState <- useState initialState
    pure $ render state props


render :: State -> Props -> JSX
render state props = DOM.div
       { className: "mosaico grid"
       , children:
           [ Header.topLine
           , state.headerComponent { router: Nothing }
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
