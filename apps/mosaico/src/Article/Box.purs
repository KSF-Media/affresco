module Mosaico.Article.Box where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe)
import React.Basic.Classic (Component, JSX, createComponent, make)
import React.Basic.DOM as R

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
      R.div
      { className: "mosaico--article--boxinfo border-color-" <> self.props.brand
      , children:
        [ R.h3_ [ R.text (fold  self.props.headline) ]
        , R.h2_ [ R.text (fold  self.props.title) ]
        , R.div
          { className: "boxinfo--body"
          , children: map (\p -> R.p { dangerouslySetInnerHTML: { __html: p } }) self.props.content
          }
        , R.div
          -- { onClick: self.setState { expanded: not self.state.expanded }
          { className: "expand"
          , children:
            [ R.div
              { className: "color-" <> self.props.brand
              , children: [ R.span_ [ R.text "Vik ut" ] ]
              }
            ]
          }
      ] }
  }
