module MosaicoServer where

import Prelude

import KSF.Paper as Paper
import Lettera.Models (ArticleStub, Category)
import Mosaico.Header as Header
import Mosaico.Paper (mosaicoPaper)
import Mosaico.MostReadList as MostReadList
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, useState, (/\))
import React.Basic.Hooks as React
import Routing.PushState (PushStateInterface)
import Simple.JSON (write)

type Props =
  { mainContent :: MainContent
  , mostReadArticles :: Array ArticleStub
  , categoryStructure :: Array Category
  }

type State =
  { headerComponent :: Header.Props -> JSX
  , mostReadListComponent :: MostReadList.Props -> JSX
  }

data MainContent
  = ArticleContent JSX
  | FrontpageContent JSX
  | TagListContent JSX
  | StaticPageContent JSX

fromMainContent :: MainContent -> JSX
fromMainContent (ArticleContent jsx) = jsx
fromMainContent (FrontpageContent jsx) = jsx
fromMainContent (TagListContent jsx) = jsx
fromMainContent (StaticPageContent jsx) = jsx

app :: Component Props
app = do
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
          { headerComponent
          , mostReadListComponent
          }
    state /\ _setState <- useState initialState
    pure $ render emptyRouter state props


render :: PushStateInterface -> State -> Props -> JSX
render router state props = DOM.div
       { className: "mosaico grid"
       , id: Paper.toString mosaicoPaper
       , children:
           [ Header.topLine
           , state.headerComponent { router
                                   , categoryStructure: props.categoryStructure
                                   , onCategoryClick: const $ pure unit
                                   }
           , Header.mainSeparator
           , fromMainContent props.mainContent
           , DOM.footer
               { className: "mosaico--footer"
               , children:
                  [ DOM.text "footer" ]
               }
           , case props.mainContent of
                 FrontpageContent _ -> aside
                 TagListContent _ -> aside
                 _ -> mempty
           ]
       }
  where
    aside =
      DOM.aside
        { className: "mosaico--aside"
        , children:
            [ state.mostReadListComponent
                { mostReadArticles: props.mostReadArticles
                , onClickHandler: const $ pure unit
                }
            ]
        }
