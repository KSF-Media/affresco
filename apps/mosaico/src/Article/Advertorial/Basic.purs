module Mosaico.Article.Advertorial.Basic where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import KSF.Paper (Paper)
import Lettera.Models (Article, ArticleStub, BodyElement(..), FullArticle(..), Image, LocalDateTime(..), Tag(..), tagToURIComponent)
import Mosaico.Article.Box (box)
import Mosaico.Article.Image as Image
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component)

type Props =
  { article :: Article
  , paper :: Paper
  }

component :: Component Props
component = do
  imageComponent <- Image.component
  React.component "Advertorial" $ \props -> React.do
    pure $ render imageComponent props

render :: (Image.Props -> JSX) -> Props -> JSX
render imageComponent props =
  let title = props.article.title
      mainImage = props.article.mainImage
      body = map renderElement props.article.body
  in DOM.article
    { children:
      [ DOM.div
        { className: "advertorial__top-bar"
        , children:
          [ DOM.span_ [ DOM.text "Annos: Company name"]
          ]
        }
      , DOM.h1
        { className: "advertorial__headline"
        , children: [ DOM.text title ]
        }
      , DOM.section
        { className: "advertorial__preamble"
        , children: [ DOM.text $ fromMaybe mempty props.article.preamble ]
        }
      , DOM.img
        { src: foldMap _.url mainImage
        , className: "article-element__full-width-mobile"
        }
      , DOM.div
          { className: "mosaico-article__body "
          , children: body
          }
      ]
    }
  where
    renderElement :: BodyElement -> JSX
    renderElement el = case el of
      Html content ->
        -- Can't place div's or blockquotes under p's, so place them under div.
        -- This is usually case with embeds
        let domFn = if isDiv content || isBlockquote content then DOM.div else DOM.p
        in domFn
           { dangerouslySetInnerHTML: { __html: content }
           , className: block <> " " <> block <> "__html"
           }
      Headline str -> DOM.h4
        { className: block <> " " <> block <> "__subheadline"
        , children: [ DOM.text str ]
        }
      Image image -> imageComponent
          { clickable: true
          , main: false
          , params: Just "&width=640&q=90"
          , image
          }
      Box boxData ->
        DOM.div
          { className: block <> " " <> block <> "__factbox"
          , children:
              [ box
                  { headline: boxData.headline
                  , title: boxData.title
                  , content: boxData.content
                  , paper: Nothing
                  }
              ]
          }
      Footnote footnote -> DOM.p
          { className: block <> " " <> block <> "__footnote"
          , children: [ DOM.text footnote ]
          }
      Quote { body, author } -> DOM.figure
          { className: block <> " " <> block <> "__quote"
          , children:
              [ DOM.blockquote_ [ DOM.text body ]
              , foldMap (DOM.figcaption_ <<< pure <<< DOM.text) author
              ]
          }
      Question question -> DOM.p
          { className: block <> " " <> block <> "__question"
          , children: [ DOM.text question ]
          }
      -- Advertorials don't list related articles or ads
      Related related -> mempty
      Ad contentUnit -> mempty
      where
        block = "article-element"
        isDiv = isElem "<div"
        isBlockquote = isElem "<blockquote"
        -- Does the string start with wanted element
        isElem elemName elemString =
          Just 0 == String.indexOf (Pattern elemName) elemString
