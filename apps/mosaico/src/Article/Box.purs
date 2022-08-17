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
    { className: "bg-gray-50 border-b-2 border-t-4 border-brand [.article-element\\_\\_reviewbox>&]:border [.article-element\\_\\_reviewbox>&]:border-gray-100"
    , children:
        [ DOM.header
            { className: "p-4 border-b border-gray-100"
            , children:
                [ DOM.h3
                    { className: "m-0 mt-3 text-2xl font-medium font-duplexsans"
                    , children: [ DOM.text $ fold props.headline ]
                    }
                , foldMap
                  (\x -> DOM.h2
                           { className: "m-0 text-xl font-medium font-duplexsans"
                           , dangerouslySetInnerHTML: { __html: x }
                           }) props.title
                ]
            }
        , if props.expanded
          then DOM.div
            { className: "text-base font-light font-roboto"
            , children:
                [ DOM.div
                    { className: "p-4"
                    , children: map (\p -> DOM.div
                                             { className: "mb-3"
                                             , dangerouslySetInnerHTML: { __html: p }
                                             }) props.content
                    }
                ]
            }
          else DOM.div
            { className: "overflow-hidden relative h-40 text-base font-light font-roboto"
            , children:
                [ DOM.div
                    { className: "absolute z-10 p-4"
                    , children: map (\p -> DOM.div
                                             { className: "mb-3"
                                             , dangerouslySetInnerHTML: { __html: p }
                                             }) props.content
                    }
                , DOM.div { className: "absolute z-20 w-full h-40 t-0 boxinfo-fader-gradient" }
                ]
            }
        , DOM.footer
            { onClick: setExpanded
            , className: "text-center cursor-pointer boxinfo__toggle"
            , children: guard (not props.expanded)
                    [ DOM.a { className: "text-base font-semibold font-roboto"
                            , children:  [ DOM.text "Vik ut ▼" ]
                            } ]
            }
        ]
    }
