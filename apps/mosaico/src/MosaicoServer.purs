module MosaicoServer where

import Prelude

import Data.Maybe (Maybe)
import KSF.Paper as Paper
import KSF.User (User)
import Lettera.Models (ArticleStub, Category, Tag, categoriesMap)
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
  , mostReadArticles :: JSX
  , categoryStructure :: Array Category
  , user :: Maybe User
  }

data MainContent
  = ArticleContent JSX
  | FrontpageContent JSX
  | HtmlFrontPageContent JSX
  | TagListContent Tag JSX
  | StaticPageContent String JSX
  | MenuContent JSX

fromMainContent :: MainContent -> JSX
fromMainContent (ArticleContent jsx) = jsx
fromMainContent (FrontpageContent jsx) = jsx
fromMainContent (HtmlFrontPageContent jsx) = jsx
fromMainContent (TagListContent _ jsx) = jsx
fromMainContent (StaticPageContent _ jsx) = jsx
fromMainContent (MenuContent jsx) = jsx

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
           , fromMainContent props.mainContent
           , DOM.footer
               { className: "mosaico--footer"
               , children:
                  [ DOM.text "footer" ]
               }
           , case props.mainContent of
                 FrontpageContent _ -> aside
                 TagListContent _ _ -> aside
                 _ -> mempty
           ]
       }
  where
    aside =
      DOM.aside
        { className: "mosaico--aside"
        , children: [ props.mostReadArticles ]
        }
