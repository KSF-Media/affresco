module Mosaico.Article.Advertorial.Basic where

import Prelude

import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Lettera.Models (Article, Image)
import Mosaico.Article.Image as Image
import Mosaico.Article as Article
import React.Basic (fragment)
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX, Component)
import React.Basic.Hooks as React

type Props =
  { article :: Article
  , imageProps :: Maybe (Image -> Image.Props)
  , advertorialClassName :: Maybe String
  }

render :: (Image.Props -> JSX) -> Props -> JSX
render imageComponent { article, imageProps, advertorialClassName } =
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
               , DOM.ul
                   { className: "mosaico-article__some advertorial-some"
                   , children: map Article.mkShareIcon [ "facebook", "twitter", "linkedin", "whatsapp", "mail" ]
                   }
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
               , foldMap
                   (\image -> imageComponent $ (fromMaybe defaultImageProps imageProps) image)
                   article.mainImage
               , DOM.div
                   { className: "mosaico-article__main"
                   , children:
                       [ DOM.div
                           { className: "mosaico-article__body"
                           , children: map (Article.renderElement Nothing imageComponent Nothing) article.body
                           }
                       ]

                   }
               , DOM.div { className: "mosaico-article__aside" }
               ]
           }
       ]

defaultImageProps image =
   { clickable: true
   , main: true
   , params: Just "&width=960&height=540&q=90"
   , image
   , fullWidth: false
   }
