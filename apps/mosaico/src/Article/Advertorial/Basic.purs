module Mosaico.Article.Advertorial.Basic where

import Prelude

import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Lettera.Models (Article)
import Mosaico.Article.Image as Image
import Mosaico.Article as Article
import React.Basic (fragment)
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX, Component)
import React.Basic.Hooks as React

type Props = { article :: Article }

component :: Component Props
component = do
  imageComponent <- Image.component
  React.component "AdvertorialBasic" \props -> React.do
    pure $ render imageComponent props

render :: (Image.Props -> JSX) -> Props -> JSX
render imageComponent { article } =
  let companyName details
        | "companyName" <- details.title = fold details.description
        | otherwise = mempty
  in fragment
       [ DOM.span
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
           , className: "mosaico-article"
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
                       { className: "advertorial__tag-n-share"
                       , children:
                           [ DOM.div
                               { className: "advertorial-badge premium-badge"
                               , children: [ DOM.span_ [ DOM.text "Annons" ] ]
                               }
                           ]
                       }
                   ]
               , foldMap
                   (\image -> imageComponent
                              { clickable: true
                              , main: true
                              , params: Just "&width=960&height=540&q=90"
                              , image
                              })
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
