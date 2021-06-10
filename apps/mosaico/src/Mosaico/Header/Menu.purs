module Mosaico.Header.Menu where

import Prelude

import Data.Array (concat, foldl)
import Data.List (List, (:), concat, foldl)
import Data.List (List, (:), concat, foldl) as List
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
                  , children: topSections <>
                              middleSections <>
                              bottomSections
                  }
              ]
  }
  where
    topSections = uncurry defaultSection <$> [ ["grid-row-1", "grid-col-2"] /\ "E-TIDNINGEN"
                                             , ["grid-row-1", "grid-col-3"] /\ "KUNDERSVICE"
                                             , ["grid-row-1", "grid-col-4"] /\ "ANNONSERA"
                                             , ["grid-row-1", "grid-col-5"] /\ "ANNAT VIKTIGT"
                                             ]

    middleSections = uncurry defaultSection <$> [ ["grid-row-3", "grid-col-2"] /\ "STARTSIDAN"
                                                , ["grid-row-3", "grid-col-3"] /\ "OPINION"
                                                , ["grid-row-3", "grid-col-4"] /\ "KULTUR"
                                                , ["grid-row-3", "grid-col-5"] /\ "SPORT"
                                                , ["grid-row-3", "grid-col-6"] /\ "ANNAT"
                                                ]

    bottomSections = uncurry greySection <$> [ ["grid-row-5", "grid-col-2"] /\ "KONTAKT"
                                             , ["grid-row-5", "grid-col-3"] /\ "ANNONSERA"
                                             , ["grid-row-5", "grid-col-4"] /\ "KUNDSERVICE"
                                             , ["grid-row-5", "grid-col-5"] /\ "KUNDSERVICE"
                                             ]

    defaultSection :: Array String -> String -> JSX
    defaultSection = section mempty

    greySection :: Array String -> String -> JSX
    greySection = section [ graySectionClass ]

    section :: List.List String -> List.List String -> String -> JSX
    section modifiers positionClasses name = DOM.div
      { className: unwords [ sectionClass, unwords modifiers, unwords positionClasses ]
      , children: [ DOM.a_ [ DOM.text name ] ]
      }

    unwords :: List.List String -> String
    unwords = trim <<< List.foldl (\a w -> a <> " " <> w) mempty

    zipWith3 :: forall a b c d. (a->b->c->d) -> List.List a -> List.List b -> List.List c -> List.List d
    zipWith3 z = go
      where
        go (a:as) (b:bs) (c:cs) = z a b c : go as bs cs
        go _ _ _                = []

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