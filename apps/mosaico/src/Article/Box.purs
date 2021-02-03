module Mosaico.Article.Box where

import Prelude

import Effect.Console (log)
import Data.Foldable (fold)
import Data.Nullable (Nullable, toMaybe)
import React.Basic.Classic (Component, JSX, createComponent, make, readState)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

component :: Component Props
component = createComponent "Box"

type Props =
  { title :: Nullable String
  , headline :: Nullable String
  , content :: Array String
  , brand :: String
  }

box :: Props -> JSX
box = make component
  { initialState: { expanded: false }
  , render: \self ->
      R.div
      { className: "ksf-article--boxinfo genericBox genericBox-border-" <> self.props.brand
      , children:
        [ R.h3_ [ R.text (fold $ toMaybe self.props.headline) ]
        , R.h2_ [ R.text (fold $ toMaybe self.props.title) ]
        , R.div
          { className: "boxinfo--body"
          , children: map (\p -> R.p { dangerouslySetInnerHTML: { __html: p } }) self.props.content
          }
        , R.div
          -- { onClick: self.setState { expanded: not self.state.expanded }
          { className: "expand"
          , children:
            [ R.div { className: "expandOpacity" }
            , R.div { className: "expandOpacity2" }
            , R.div
              { className: "brandColor-" <> self.props.brand
              , children: [ R.span_ [ R.text "Vik ut" ] ]
              }
            ]
          }  
      ] }
  }