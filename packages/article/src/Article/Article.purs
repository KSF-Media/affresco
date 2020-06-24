module Article where

import Prelude

import Data.Maybe (Maybe(..))
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
  { html     :: Maybe String
  , image    :: Maybe ImageInfo
  , box      :: Maybe BoxInfo
  , headline :: Maybe String
  , footnote :: Maybe String
  , question :: Maybe String
  , quote    :: Maybe String
  }

type ImageInfo = {}
type BoxInfo = {}

type MainImage =
  { url       :: String
  , caption   :: Maybe String
  , thumb     :: Maybe String
  , alignment :: Maybe String
  , byline    :: Maybe String
  }

component :: React.Component Props
component = React.createComponent "Article"

article :: Props -> JSX
article = React.make component
 { initialState: {}, render }


render :: Self -> JSX
render self =
  DOM.div
    { className: "ksf-article"
    , children: [
      DOM.h1
        { className: "ksf-article--title"
        , children: [ DOM.text self.props.article.title ]
        }
      , DOM.div
        { className: "ksf-article--image"
        , children: [
          DOM.img { src: self.props.article.mainImage.url }
          ]  }
      , DOM.div
        { className: "ksf-article--body"
        , children: map bodyElement self.props.article.body

        }
      ]
    }
  where
    bodyElement :: BodyElement -> JSX
    bodyElement el
      | Just html      <- el.html = DOM.p_ [ DOM.text html ]
      | Just imageInfo <- el.image = mempty
      | Just boxInfo   <- el.box = mempty
      | Just asd <- el.headline = mempty
      | Just asd <- el.footnote = mempty
      | Just ads <- el.question = mempty
      | Just quote <- el.quote = mempty
      | otherwise = mempty

    -- DOM.p
      --   { className: "ksf-article--body-element"
      --   , dangerouslySetInnerHTML: { __html: el.html }
      --   }
