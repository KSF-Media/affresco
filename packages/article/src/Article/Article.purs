module Article where

import Prelude

import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import Data.Maybe (Maybe)

type Self = React.Self Props State

type Props =
  { article :: Article }

type State = {}

type Article = 
  { title     :: String
  , body      :: Array BodyElement
  , mainImage :: MainImage }

type BodyElement = { html :: String }
type MainImage = 
  { url       :: String
  , caption   :: Maybe String
  , thumb     :: Maybe String
  , alignment :: Maybe String
  , byline    :: Maybe String }

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
    bodyElement el =
      DOM.p
        { className: "ksf-article--body-element"
        , dangerouslySetInnerHTML: { __html: el.html }
        }

