module Mosaico.Article.Box where

import Prelude

import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe)
import Data.Monoid (guard)
import Data.String (joinWith, length)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, useState', (/\))

component :: Component Props
component = do
  React.component "Box" $ \props -> React.do
    expanded /\ setExpanded <- useState' props.expanded
    pure $ render (capture_ $ setExpanded true) (props { expanded = expanded })

type Props =
  { title :: Maybe String
  , headline :: Maybe String
  , content :: Array String
  , expanded :: Boolean
  }

autoExpand :: Array String -> Boolean
autoExpand a = (length $ joinWith mempty a) < 600

render :: EventHandler -> Props -> JSX
render setExpanded props =
  DOM.section
    { className: "boxinfo"
    , children:
        [ DOM.header
            { className: "boxinfo__header"
            , children:
                [ DOM.h3
                    { className: "boxinfo__label"
                    , children: [ DOM.text $ fold props.headline ]
                    }
                , foldMap
                  (\x -> DOM.h2
                           { className: "boxinfo__title"
                           , dangerouslySetInnerHTML: { __html: x }
                           }) props.title
                ]
            }
        , DOM.div
            { className: "boxinfo__body" <> guard props.expanded "--expanded"
            , children:
                [ DOM.div
                    { className: "boxinfo-body__content"
                    , children: map (\p -> DOM.div { dangerouslySetInnerHTML: { __html: p } }) props.content
                    }
                , guard (not props.expanded) $ DOM.div { className: "boxinfo-body__fader" }
                ]
            }
        , DOM.footer
            { onClick: setExpanded
            , className: "boxinfo__toggle"
            , children: guard (not props.expanded) [ DOM.a_ [ DOM.text "Vik ut ▼" ] ]
            }
        ]
    }
