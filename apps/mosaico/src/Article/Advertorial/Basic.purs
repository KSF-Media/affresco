module Mosaico.Article.Advertorial.Basic where

import Prelude

import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Lettera.Models (Article, Image)
import Mosaico.Article.Box as Box
import Mosaico.Article.Image as Image
import Mosaico.Article as Article
import Mosaico.Share as Share
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Basic.Hooks (Component)

type Props =
  { article :: Article
  , imageProps :: Maybe (Image -> Image.Props)
  , advertorialClassName :: Maybe String
  }

component :: Component Props
component = do
  imageComponent <- Image.component
  boxComponent <- Box.component
  React.component "Basic" $ \props -> React.do
    pure $ render imageComponent boxComponent props

render :: (Image.Props -> JSX) -> (Box.Props -> JSX) -> Props -> JSX
render imageComponent boxComponent { article, imageProps, advertorialClassName } =
  let companyName details
        | "companyName" <- details.title = fold details.description
        | otherwise = mempty
  in fragment
       [ DOM.div
           { className: "advertorial-top-banner"
           , children:
               [ DOM.span
                   { className: "advertorial-top-banner__company"
                   , children: [ DOM.text $ "ANNONS: " <> foldMap (String.toUpper <<< companyName) article.articleTypeDetails ]
                   }
               , Share.articleShareButtons article.title article.shareUrl
               ]
           }
       , DOM.article
           { id: "BRAND-NEUTRAL"
           , className: "mosaico-article " <> fold advertorialClassName
           , children:
               [ DOM.header_
                   [ DOM.h1
                       { className: "mosaico-article__headline"
                       , children: [ DOM.text article.title ]
                       }
                   , DOM.section
                       { className: "mosaico-article__preamble"
                       , children: [ DOM.text $ fold article.preamble ]
                       }
                   , DOM.div
                       { className: "advertorial-badge"
                       , children:
                           [ DOM.div
                               { className: "premium-badge"
                               , children: [ DOM.span_ [ DOM.text "Annons" ] ]
                               }
                           ]
                       }
                   ]
               , foldMap (imageComponent <<< fromMaybe defaultImageProps imageProps) article.mainImage
               , DOM.div
                   { className: "mosaico-article__main"
                   , children:
                       [ DOM.div
                           { className: "mosaico-article__body"
                           , children:
                               [ DOM.section
                                   { className: "article-content"
                                   , children: map (Article.renderElement imageComponent boxComponent Nothing) article.body
                                   }
                               ]
                           }
                       ]

                   }
               , DOM.div { className: "mosaico-article__aside" }
               ]
           }
       ]

defaultImageProps :: Image -> Image.Props
defaultImageProps image =
   { clickable: true
   , main: true
   , params: Nothing
   , image
   , fullWidth: false
   }
