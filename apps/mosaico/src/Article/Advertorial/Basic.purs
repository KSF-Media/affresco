module Mosaico.Article.Advertorial.Basic where

import Prelude

import Data.Foldable (fold, find, foldMap)
import React.Basic.DOM as DOM
import React.Basic (fragment)
import Data.Maybe (Maybe)
import Data.String as String
import React.Basic.Hooks (JSX)

import Lettera.Models (Article)

type Props = { article :: Article }

render :: Props -> JSX
render { article } =
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
               ]
           }
       , DOM.article
           { className: "mosaico-article"
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
               ]
           }
        ]
