module Article where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.RecordToSum as Record
import Data.Generic.Rep.Show (genericShow)
import Data.Either (Either(..))
import Data.Nullable (Nullable)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM


type Self = React.Self Props State

type Props =
  { article :: Article }

type Article =
  { title     :: String
  , body      :: Array BodyElementJS
  , mainImage :: Image
  }

type BodyElementJS =
  { html     :: Nullable String
  , image    :: Nullable Image
  , box      :: Nullable BoxInfo
  , headline :: Nullable String
  , footnote :: Nullable String
  , question :: Nullable String
  , quote    :: Nullable String
  }

type State =
  { body :: Array (Either String BodyElement) }

data BodyElement
  = Html String
  | Image Image
  | Box BoxInfo
  | Headline String
  | Footnote String
  | Question String
  | Quote String
derive instance bodyElementGeneric :: Generic BodyElement _
instance bodyElementShow :: Show BodyElement where show = genericShow

type BoxInfo = {}

type Image =
  { url       :: String
  , caption   :: Nullable String
  , thumb     :: Nullable String
  , alignment :: Nullable String
  , byline    :: Nullable String
  }

component :: React.Component Props
component = React.createComponent "Article"

article :: Props -> JSX
article props = React.make
  component
  { initialState: { body: map Record.toSum props.article.body }, render }
  props


render :: Self -> JSX
render { props, state } =
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
          { className: "ksf-article--body-element"
          , dangerouslySetInnerHTML: { __html: content }
          }
        other -> DOM.p_ [ DOM.text $ show other ]