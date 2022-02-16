module MosaicoServer where

import Prelude

import Data.Maybe (Maybe)
import KSF.Paper as Paper
import KSF.User (User)
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
  , user :: Maybe User
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
render router props = DOM.div
       { className: "mosaico grid"
       , id: Paper.toString mosaicoPaper
       , children:
           [ Header.topLine
           , Header.render { router
                           , categoryStructure: props.categoryStructure
                           , catMap: categoriesMap props.categoryStructure
                           , onCategoryClick: const mempty
                           , user: props.user
                           , onLogin: pure unit
                           }
           , Header.mainSeparator
           , props.mainContent.content
           , footer mempty
           , case props.mainContent.type of
                 FrontpageContent -> aside
                 TagListContent _ -> aside
                 _ -> mempty
           ]
       }
  where
    aside =
      DOM.aside
        { className: "mosaico--aside"
        , children:
          [ MostReadList.render { mostReadArticles: props.mostReadArticles, onClickHandler: const $ pure unit }
          , LatestList.render { latestArticles: props.latestArticles, onClickHandler: const $ pure unit }
          ]
        }
