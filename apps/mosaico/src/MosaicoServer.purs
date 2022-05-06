module MosaicoServer where

import Prelude

import Data.Maybe (Maybe(Nothing))
import KSF.Paper as Paper
import Lettera.Models (ArticleStub, Category, Tag, categoriesMap)
import Mosaico.Footer (footer)
import Mosaico.Header as Header
import Mosaico.Paper (mosaicoPaper)
import Mosaico.MostReadList as MostReadList
import Mosaico.LatestList as LatestList
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX)
import Routing.PushState (PushStateInterface)
import Simple.JSON (write)

type Props =
  { mainContent :: MainContent
  , mostReadArticles :: Array ArticleStub
  , latestArticles :: Array ArticleStub
  , categoryStructure :: Array Category
  }

type MainContent =
  { content :: JSX
  , type :: MainContentType
  }

data MainContentType
  = ArticleContent
  | FrontpageContent
  | HtmlFrontpageContent
  | TagListContent Tag
  | EpaperContent
  | StaticPageContent String
  | ProfileContent
  | MenuContent

app :: Props -> JSX
app props =
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
   in
     render emptyRouter props


render :: PushStateInterface -> Props -> JSX
render router props = DOM.div_
    [ DOM.div
       { className: "mosaico grid " <> menuOpen
       , id: Paper.toString mosaicoPaper
       , children:
           [ Header.render 0 0
             { router
             , categoryStructure: props.categoryStructure
             , catMap: categoriesMap props.categoryStructure
             , onCategoryClick: const mempty
             , user: Nothing
             , onLogin: mempty
             , onProfile: mempty
             , onStaticPageClick: mempty
             }
           , props.mainContent.content
           , footer mosaicoPaper mempty
           , case props.mainContent.type of
                 FrontpageContent -> aside
                 TagListContent _ -> aside
                 _ -> mempty
           ]
       }
    ]
  where
    aside =
      DOM.aside
        { className: "mosaico--aside"
        , children:
          [ MostReadList.render { mostReadArticles: props.mostReadArticles, onClickHandler: const mempty }
          , LatestList.render { latestArticles: props.latestArticles, onClickHandler: const mempty }
          ]
        }
    menuOpen = case props.mainContent.type of
      MenuContent -> "menu-open"
      _           -> mempty
