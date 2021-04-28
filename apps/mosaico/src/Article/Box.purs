module Mosaico.Article.Box where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Data.Monoid (guard)
import React.Basic.Classic (Component, JSX, createComponent, make)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)


component :: Component Props
component = createComponent "Box"

type Props =
  { title :: Maybe String
  , headline :: Maybe String
  , content :: Array String
  , brand :: String
  }

box :: Props -> JSX
box = make component
  { initialState: { expanded: false }
  , render: \self ->
      DOM.div
        { className: "mosaico--article--boxinfo border-" <> self.props.brand
        , children:
            [ DOM.div
              { className: "boxinfo-header"
              , children:
                  [ DOM.h3_ [ DOM.text $ fold self.props.headline ]
                  , DOM.h2_ [ DOM.text $ fold self.props.title ]
                  ]
              }
            , DOM.div
              { className: "boxinfo--body" <> guard self.state.expanded " expanded"
              , children:
                  [ DOM.div
                    { className: "content"
                    , children: map (\p -> DOM.p { dangerouslySetInnerHTML: { __html: p } }) self.props.content
                    }
                  , DOM.div { className: "fader" }
                  ]
              }
            , DOM.div
              { onClick: capture_ $ self.setState \_ -> { expanded: not self.state.expanded }
              , className: "boxinfo-toggle"
              , children: guard (not self.state.expanded) $
                  [ DOM.a
                    { className: "color-" <> self.props.brand
                    , children: [ DOM.text "Vik ut ▼" ]
                    }
                  ]
              }
            ]
        }
  }
