module Mosaico.Article.Image where

import Prelude

import Data.Maybe (Maybe)
import Data.Foldable (fold)
import Lettera.Models (Image)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, useState, (/\))


type Props =
  { clickable :: Boolean
  , main      :: Boolean
  , params    :: Maybe String
  , image     :: Image
  }

component :: Component Props
component = do
  React.component "Image" $ \props -> React.do
    opened /\ setOpened <- useState false
    let toggleOpen = capture_ $ setOpened not
        openedImage = if opened then articleFullScreen toggleOpen props else mempty
    pure $ (render toggleOpen props) <> openedImage

render :: EventHandler -> Props -> JSX
render onClick props =
  if props.main then articleMainImage onClick props else articleImage onClick props

articleImage :: EventHandler -> Props -> JSX
articleImage onClick props =
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
    , onClick
    }
  where
    img = props.image
    params = fold props.params
    caption = fold img.caption
    byline  = fold img.byline

articleMainImage :: EventHandler -> Props -> JSX
articleMainImage onClick props =
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
    , onClick
    }
  where
    img = props.image
    params = fold props.params
    caption = fold img.caption
    byline  = fold img.byline

articleFullScreen :: EventHandler -> Props -> JSX
articleFullScreen onClick props =
  DOM.div
    { className: "mosaico-article__focus-image"
    , children:
        [ DOM.div
            { className: "wrapper"
            , children:
                [ DOM.img
                    { src: props.image.url <> "&width=1600&height=1065&q=75"
                    }
                ]
            }
        ]
    , onClick
    }
