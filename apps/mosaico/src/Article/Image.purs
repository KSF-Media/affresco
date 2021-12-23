module Mosaico.Article.Image where

import Prelude

import Data.Maybe (Maybe)
import Data.Foldable (fold)
import Lettera.Models (Image)
import React.Basic (JSX)
import React.Basic.DOM as DOM


type Props =
  { clickable :: Boolean
  , params    :: Maybe String
  , image     :: Image
  }

articleImage :: Props -> JSX
articleImage props =
  DOM.div
    { className: "mosaico-article__image"
    , children:
        [ DOM.img
            { src: img.url <> params
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
    img = props.image
    params = fold props.params
    caption = fold img.caption
    byline  = fold img.byline

articleMainImage :: Props -> JSX
articleMainImage props =
  DOM.div
    { className: "mosaico-article__main-image"
    , children:
        [ DOM.div
            { className: "wrapper"
            , children:
                [ DOM.img
                    { src: img.url <> params
                    , title: caption
                    }
                ]
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
    img = props.image
    params = fold props.params
    caption = fold img.caption
    byline  = fold img.byline
