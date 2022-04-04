module Mosaico.Article.Advertorial.Basic where

import Prelude

import Data.Foldable (fold)
import React.Basic.DOM as DOM
import React.Basic (fragment)
import React.Basic.Hooks (JSX)

import Lettera.Models (Article)

type Props = { article :: Article }

render :: Props -> JSX
render { article } = fragment [ DOM.span { className: "advertorial-top-banner", children: [ DOM.text "ayoo" ] },

    DOM.article
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
                ]
            ]
        }
]
