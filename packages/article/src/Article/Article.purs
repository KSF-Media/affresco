module Article where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM

type Self = React.Self Props State

type Props =
  { article :: Article }

type State = { }

type Article =
  { title     :: String
  , body      :: Array BodyElement
  , mainImage :: MainImage
  }

type BodyElement =
  { html     :: Nullable String
  , image    :: Nullable ImageInfo
  , box      :: Nullable BoxInfo
  , headline :: Nullable String
  , footnote :: Nullable String
  , question :: Nullable String
  , quote    :: Nullable String
  }

type ImageInfo = {}
type BoxInfo = {}

type MainImage =
  { url       :: String
  , caption   :: Nullable String
  , thumb     :: Nullable String
  , alignment :: Nullable String
  , byline    :: Nullable String
  }

component :: React.Component Props
component = React.createComponent "Article"

article :: Props -> JSX
article = React.make component
 { initialState: {}, render }


render :: Self -> JSX
render { props } =
  DOM.div
    { className: "ksf-article"
    , children: [
      DOM.h1
        { className: "ksf-article--title"
        , children: [ DOM.text props.article.title ]
        }
      , DOM.div
        { className: "ksf-article--image"
        , children: [
          DOM.img { src: props.article.mainImage.url }
          ]  }
      , DOM.div
        { className: "ksf-article--body"
        , children: map bodyElement props.article.body

        }
      ]
    }
  where
    bodyElement :: BodyElement -> JSX
    bodyElement el
      | Just html      <- toMaybe $ el.html = DOM.p_ [ DOM.text html ]
      | Just imageInfo <- toMaybe $ el.image = mempty
      | Just boxInfo   <- toMaybe $ el.box = mempty
      | Just asd <- toMaybe $ el.headline = mempty
      | Just asd <- toMaybe $ el.footnote = mempty
      | Just ads <- toMaybe $ el.question = mempty
      | Just quote <- toMaybe $ el.quote = mempty
      | otherwise = mempty

    -- DOM.p
      --   { className: "ksf-article--body-element"
      --   , dangerouslySetInnerHTML: { __html: el.html }
      --   }
