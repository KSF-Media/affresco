module Mosaico.Article where

import Prelude

import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Show.Generic (genericShow)
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Lettera.Models (Article, BodyElement(..), BoxInfo, Image)
import Data.Generic.Rep.RecordToSum as Record
import React.Basic (JSX)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import Mosaico.Article.Box (box)

type Self = React.Self Props State

type Props =
  { article :: Article
  , brand :: String
  }

type State =
  { body :: Array (Either String BodyElement) }

component :: React.Component Props
component = React.createComponent "Article"

article :: Props -> JSX
article props = React.make
  component
  { initialState: { body: map Record.toSum props.article.body }, render }
  props

renderImage :: Image -> JSX
renderImage img =
  DOM.div
    { className: "mosaico--article--image"
    , children: [
      DOM.img
        { src: img.url
        , title: caption }
      , DOM.div { className: "caption", children:
        [ DOM.text caption
        , DOM.span { className: "byline", children: [ DOM.text byline ] } ] }
    ] }
      where
        caption = fold $ img.caption
        byline = fold $ img.byline

render :: Self -> JSX
render { props, state } =
  DOM.div
    { className: "mosaico--article"
    , children: [
      DOM.div
        { className: "mosaico--tag brand-" <> props.brand
        , children: [ DOM.text $ fromMaybe "" (head props.article.tags) ]
        }
      , DOM.h1
        { className: "mosaico--article--title title"
        , children: [ DOM.text props.article.title ]
        }
      , renderImage props.article.mainImage
      , DOM.div
        { className: "mosaico--article--body"
        , children: map renderElement state.body
        }
      ]
    }
  where
    -- TODO: maybe we don't want to deal at all with the error cases
    -- and we want to throw them away?
    renderElement :: Either String BodyElement -> JSX
    renderElement = case _ of
      Left err -> mempty
      Right el -> case el of
        Html content -> DOM.p
          { dangerouslySetInnerHTML: { __html: content }
          , className: "html"
          }
        Headline str -> DOM.h4
          { className: "headline"
          , children: [ DOM.text str ]
          }
        Image img -> renderImage img
        Box boxData -> DOM.div {
          className: "factbox",
          children:
            [ box
              { headline: boxData.headline
              , title: boxData.title
              , content: boxData.content
              , brand: props.brand
              }
            ]
          }
        other -> DOM.p_ [ DOM.text $ show other ]
