module Mosaico.Header.Menu where

import Prelude

import Data.Array ((..), concat, foldl, range)
import Data.List (List, (:))
import Data.List.Lazy as List
import Data.String.Common (trim)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))

import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

type Self =
  { props :: Props
  }

type Props =
  { visible :: Boolean
  }

menuComponent :: Component Props
menuComponent = do
  component "Menu" \props -> React.do
    pure $ render { props }

render :: Self -> JSX
render self@{ props: { visible } } = DOM.div
  { className: menuClass <>
      if visible then
        " " <> menuModifierClass
      else
        mempty
  , children: [ DOM.div
                  { className: searchClass
                  , children: [ DOM.text "search" ]
                  }
              , DOM.div
                  { className: menuContentClass <> " grid-row-2 grid-colspan-3"
                  , children: sections
                  }
              ]
  }
  where
    sections :: Array JSX
    sections = completeSections
      where
        -- produces the final list of sections
        completeSections = concat <<< List.toUnfoldable <<< (<$>) List.toUnfoldable <<< List.zipWith (List.zipWith ($)) fixedRowsAndColumns $ List.fromFoldable [topSectionTitles, middleSectionTitles, bottomSectionTitles ]
        -- produces a list of lists, each list contains one row of section generating functions, eeach with row and column class arguments assigned: 
        -- [[defaultSection "grid-row-1" "grid-column-2", defaultSection "grid-row-1" "grid-column-3", ... ], [defaultSection "grid-row-2" "grid-column-2", ... ], ... ]
        fixedRowsAndColumns = List.zipWith (<$>) fixedRows $ List.repeat gridColumns
        -- Produces a list of section generating functions that have the row class argument assigned: [ defaultSection "grid-row-1", .. ]
        fixedRows = List.zipWith ($) (List.fromFoldable [ defaultSection, defaultSection, greySection ]) gridRows

        -- CSS classes for grid rows positions
        gridRows = List.fromFoldable $ ((<>) "grid-row-" <<< show) <$> [ 1, 3, 5 ]
        -- CSS classes for grid columns positions
        gridColumns = List.fromFoldable $ ((<>) "grid-col-" <<< show) <$> range 2 6

        topSectionTitles = List.fromFoldable ["E-TIDNINGEN", "KUNDERSVICE", "ANNONSERA", "ANNAT VIKTIGT"]
        middleSectionTitles = List.fromFoldable ["STARTSIDAN", "OPINION", "KULTUR", "SPORT", "ANNAT"]
        bottomSectionTitles = List.fromFoldable["KONTAKT", "ANNONSERA", "KUNDSERVICE", "KUNDSERVICE"]

    defaultSection :: String -> String -> String -> JSX
    defaultSection = section mempty

    greySection :: String -> String -> String -> JSX
    greySection = section [ graySectionClass ]

    section :: Array String -> String -> String -> String -> JSX
    section modifiers rowClass columnClass name = DOM.div
      { className: unwords [ sectionClass, unwords modifiers, rowClass, columnClass ]
      , children: [ DOM.a_ [ DOM.text name ] ]
      }

    unwords :: Array String -> String
    unwords = trim <<< foldl (\a w -> a <> " " <> w) mempty

    {- zipWith3 :: forall a b c d. (a->b->c->d) -> List.List a -> List.List b -> List.List c -> List.List d
    zipWith3 z = go
      where
        go (a:as) (b:bs) (c:cs) = z a b c : go as bs cs
        go _ _ _                = List.fromFoldable []-}

    headerBlock = "mosaico-header"
    menuElement = "__menu"
    visibleModifier = "--visible"
    menuClass = headerBlock <> menuElement
    menuModifierClass = menuClass <> visibleModifier
    searchElement = "__search"
    searchClass = headerBlock <> searchElement
    menuContentElement = "__menu-content"
    menuContentClass = headerBlock <> menuContentElement
    sectionElement = "__section"
    sectionClass = headerBlock <> sectionElement
    grayModifier = "--gray"
    graySectionClass = sectionClass <> grayModifier
    subsectionElement = "__subsection"
    subsectionClass = headerBlock <> subsectionElement