module Mosaico.Article where

import Prelude

import Control.Alt ((<|>))
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep.RecordToSum as Record
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Lettera.Models (Article, BodyElement(..), Image, ArticleStub)
import Mosaico.Article.Box (box)
import React.Basic (JSX)
import React.Basic.Classic as React
import React.Basic.DOM as DOM

type Self = React.Self Props State

type Props =
  { brand :: String
  , affArticle :: Aff Article
  , articleStub :: Maybe ArticleStub
  }

type State =
  { body :: Array (Either String BodyElement)
  , article :: Maybe Article
  }

component :: React.Component Props
component = React.createComponent "Article"

article :: Props -> JSX
article props = React.make
  component
  { initialState: { body: [], article: Nothing }, render, didMount }
  props

didMount :: Self -> Effect Unit
didMount self = do
  Aff.launchAff_ do
    a <- self.props.affArticle
    liftEffect $ self.setState \s -> s { article = Just a,  body = map Record.toSum a.body }

renderImage :: Image -> JSX
renderImage img =
  DOM.div
    { className: "mosaico--article--image"
    , children:
        [ DOM.img
            { src: img.url
            , title: caption
            }
        , DOM.div
            { className: "caption"
            , children:
                [ DOM.text caption
                , DOM.span
                    { className: "byline"
                    , children: [ DOM.text byline ]
                    }
                ]
            }
      ]
    }
  where
    caption = fold $ img.caption
    byline = fold $ img.byline

render :: Self -> JSX
render { props, state } =
  let title = fromMaybe mempty $ map _.title props.articleStub <|> map _.title state.article
      tags = fromMaybe mempty $  map _.tags props.articleStub <|> map _.tags state.article
      mainImage = (_.listImage =<< props.articleStub) <|> (_.mainImage =<< state.article)
  in DOM.div
    { className: "mosaico--article"
    , children: [
      DOM.div
        { className: "mosaico--tag color-" <> props.brand
        , children: [ DOM.text $ fromMaybe "" (head tags) ]
        }
      , DOM.h1
        { className: "mosaico--article--title title"
        , children: [ DOM.text title ]
        }
      , foldMap renderImage mainImage
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
