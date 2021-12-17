module Mosaico.Article.Box where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Data.Monoid (guard)
import Data.String (joinWith, length)
import KSF.Paper (Paper)
import KSF.Paper as Paper
import React.Basic.Classic (Component, JSX, createComponent, make)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)


component :: Component Props
component = createComponent "Box"

type Props =
  { title :: Maybe String
  , headline :: Maybe String
  , content :: Array String
  , paper :: Paper
  }

autoExpand :: Array String -> Boolean
autoExpand a = (length $ joinWith mempty a) < 600

box :: Props -> JSX
box = make component
  { initialState: { expanded: false }
  , didMount: \self ->
      self.setState \_ -> { expanded: autoExpand self.props.content }
  , render: \self ->
      DOM.section
        { className: "boxinfo border-" <> Paper.cssName self.props.paper
        , children:
            [ DOM.header
              { className: "boxinfo__header"
              , children:
                  [ DOM.h3
                      { className: "boxinfo__label"
                      , children: [ DOM.text $ fold self.props.headline ]
                      }   
                  , DOM.h2
                      { className: "boxinfo__title"
                      , children: [ DOM.text $ fold self.props.title ]
                      }
                  ]
              }
            , DOM.div
              { className: "boxinfo__body" <> guard self.state.expanded "--expanded"
              , children:
                  [ DOM.div
                    { className: "boxinfo-body__content"
                    , children: map (\p -> DOM.p { dangerouslySetInnerHTML: { __html: p } }) self.props.content
                    }
                  , guard (not self.state.expanded) $ DOM.div { className: "boxinfo-body__fader" }
                  ]
              }
            , DOM.footer
              { onClick: capture_ $ self.setState \_ -> { expanded: not self.state.expanded }
              , className: "boxinfo__toggle"
              , children: guard (not self.state.expanded) $
                  [ DOM.a
                    { className: "color-" <> Paper.cssName self.props.paper
                    , children: [ DOM.text "Vik ut ▼" ]
                    }
                  ]
              }
            ]
        }
  }
