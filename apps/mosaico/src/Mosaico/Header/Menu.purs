module Mosaico.Header.Menu where

import Prelude

import Data.Array (concat, cons, foldl, range)
import Data.List.Lazy as List
import Data.String.Common (trim)

import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

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
render { props: { visible } } = DOM.div
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
        -- produces the final array of sections
        completeSections = concat <<< List.toUnfoldable $ List.toUnfoldable <$> withSeparators
        -- append a separator to each list of sections
        withSeparators = List.zipWith (<>) withSubsections separators
        -- same structure as below, but the proper subsection array is now assigned to each section function
        withSubsections = List.zipWith (List.zipWith ($)) withTitles subsections
        -- same structure as below, but the proper title is now assigned to each section function
        withTitles = List.zipWith (List.zipWith ($)) fixedRowsAndColumns sectionTitles
        -- produces a list of lists, each list contains one row of section generating functions, eeach with row and column class arguments assigned:
        -- [[defaultSection "grid-row-1" "grid-column-2", defaultSection "grid-row-1" "grid-column-3", ... ], [defaultSection "grid-row-2" "grid-column-2", ... ], ... ]
        fixedRowsAndColumns = List.zipWith (<$>) fixedRows $ List.repeat gridColumns
        -- Produces a list of section generating functions that have the row class argument assigned: [ defaultSection "grid-row-1", .. ]
        fixedRows = List.zipWith ($) (List.fromFoldable [ defaultSection, defaultSection, greySection ]) gridRows

        -- CSS classes for grid rows positions
        gridRows = List.fromFoldable $ ((<>) "grid-row-" <<< show) <$> [ 1, 3, 5 ]
        -- CSS classes for grid columns positions
        gridColumns = List.fromFoldable $ ((<>) "grid-col-" <<< show) <$> range 2 6

        separators = List.fromFoldable $ List.singleton <<< separator <$> separatorRows

        separatorRows = ((<>) "grid-row-" <<< show) <$> [ 2, 4, 6 ]

        sectionTitles = List.fromFoldable [ topSectionTitles, middleSectionTitles, bottomSectionTitles ]

        topSectionTitles = List.fromFoldable ["E-TIDNINGEN", "KUNDERSVICE", "ANNONSERA", "ANNAT VIKTIGT"]
        middleSectionTitles = List.fromFoldable ["STARTSIDAN", "OPINION", "KULTUR", "SPORT", "ANNAT"]
        bottomSectionTitles = List.fromFoldable ["KONTAKT", "ANNONSERA", "KUNDSERVICE", "KUNDSERVICE"]

        subsections =  List.fromFoldable [ topSubsections, middleSubsections, bottomSubsections ]

        topSubsections = List.fromFoldable [[], [], [], []]

        middleSubsections = List.fromFoldable [ ["Lorem", "Pellentesque", "Aliquet", "Utrices"]
                                              , ["Lorem", "Pellentesque", "Aliquet", "Utrices"]
                                              , ["Lorem", "Pellentesque", "Aliquet", "Utrices"]
                                              , ["Lorem", "Pellentesque", "Aliquet", "Utrices"]
                                              , ["Lorem", "Pellentesque", "Aliquet", "Utrices"]
                                              ]

        bottomSubsections = List.fromFoldable [ ["Lorem", "Pellentesque", "Aliquet", "Utrices"]
                                              , ["Lorem", "Pellentesque", "Aliquet", "Utrices"]
                                              , ["Lorem", "Pellentesque", "Aliquet", "Utrices"]
                                              , ["Lorem", "Pellentesque", "Aliquet", "Utrices"]
                                              ]


    defaultSection :: String -> String -> String  -> Array String -> JSX
    defaultSection = section mempty

    greySection :: String -> String -> String  -> Array String -> JSX
    greySection = section [ graySectionClass ]

    section :: Array String -> String -> String -> String -> Array String -> JSX
    section modifiers rowClass columnClass name subsections = DOM.div
      { className: unwords [ sectionClass, unwords modifiers, rowClass, columnClass ]
      , children: DOM.a { className: sectionTitleClass
                        , children: [ DOM.text name ]
                        }  `cons` (subsection <$> subsections)
      }

    subsection :: String -> JSX
    subsection name = DOM.a
      { className: subsectionClass
      , children: [ DOM.text name ]
      }

    separator :: String -> JSX
    separator rowClass = DOM.hr { className: unwords [ separatorClass, rowClass, "grid-col-2", "grid-colspan-5"] }

    unwords :: Array String -> String
    unwords = trim <<< foldl (\a w -> a <> " " <> w) mempty

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

    sectionTitleElement = "__section-title"
    sectionTitleClass = headerBlock <> sectionTitleElement

    subsectionElement = "__subsection"
    subsectionClass = headerBlock <> subsectionElement

    separatorElement = "__separator"
    separatorClass = headerBlock <> separatorElement
