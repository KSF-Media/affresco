module Mosaico.Article.Advertorial.Basic where

import Prelude

import Lettera.Models (Article)
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX)

type Props = { article :: Article }

render :: Props -> JSX
render { article } =
  DOM.article
    { className: "mosaico-article"
    , children:
        [ DOM.text "ayoo" ]
        }
