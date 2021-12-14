module Mosaico.Frontpage where

import Prelude

import Data.Array (head)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (un)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import KSF.HtmlRenderer as HtmlRenderer
import KSF.Spinner (loadingSpinner)
import Lettera.Models (ArticleStub, Tag(..), tagToURIComponent)
import Mosaico.Models (ArticleFeed(..))
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component, component)

type State = { htmlRendererComponent :: HtmlRenderer.Props -> JSX }

type Props =
  { content :: Maybe ArticleFeed
  , onArticleClick :: ArticleStub -> EventHandler
  , onTagClick :: Tag -> EventHandler
  }

frontpageComponent :: Component Props
frontpageComponent = do
  htmlRendererComponent <- HtmlRenderer.htmlRendererComponent
  component "FrontpageComponent" \props -> React.do
    let initialState = { htmlRendererComponent }
    pure $ render initialState props

render :: State -> Props -> JSX
render state props =
  DOM.div
    { className: "mosaico--article-list"
    , children: maybe [loadingSpinner] $ case props.content of
        ArticleList list -> map renderListArticle list
        Html content     -> [ state.htmlRendererComponent { content } ]
    }
  
  where
    renderListArticle :: ArticleStub -> JSX
    renderListArticle a =
      DOM.div
        { className: "mosaico--list-article list-article-default"
        , onClick: props.onArticleClick a
        , _data: Object.fromFoldable $
          -- Known bug, exclude from tests
          if Just true == (contains (Pattern ":") <<< un Tag <$> head a.tags) then []
          else [ Tuple "premium" $ if a.premium then "1" else "0"
               , Tuple "uuid" $ a.uuid
               ]
        , children:
            [ DOM.span
                { children:
                    [ DOM.a
                        { href: "/artikel/" <> a.uuid
                        , className: "list-article-image"
                        , children: [ DOM.img { src: maybe "https://cdn.ksfmedia.fi/mosaico/hbl-fallback-img.png" _.url a.mainImage } ]
                        }
                    ,  DOM.div
                         { className: "list-article-liftup"
                         , children:
                             [ foldMap
                                 (\tag ->
                                     DOM.a
                                       { className: "mosaico-article__tag color-hbl"
                                       , onClick: props.onTagClick tag
                                       , href: "/tagg/" <> tagToURIComponent tag
                                       , children: [ DOM.text $ un Tag tag ]
                                       }
                                 ) $ head a.tags
                             , DOM.a
                                 { href: "/artikel/" <> a.uuid
                                 , children: [ DOM.h2_ [ DOM.text $ fromMaybe a.title a.listTitle] ]
                                 }
                             , guard a.premium $
                               DOM.div
                                 { className: "mosaico--article--meta"
                                 , children:
                                     [ DOM.div
                                         { className: "premium-badge background-hbl"
                                         , children: [ DOM.text "premium" ]
                                         }
                                     ]
                                 }
                             ]
                         }
                    ]
                }
            ]
        }
