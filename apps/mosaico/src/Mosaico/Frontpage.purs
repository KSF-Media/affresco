module Mosaico.Frontpage
  ( Frontpage(..)
  , ListFrontpageProps(..)
  , PrerenderedFrontpageProps(..)
  , render
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (head)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (un)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import KSF.HtmlRenderer (render) as HtmlRenderer
import KSF.Spinner (loadingSpinner)
import Lettera.Models (ArticleStub, Tag(..), tagToURIComponent)
import Mosaico.FallbackImage (fallbackImage)
import Mosaico.Frontpage.Models (Hook, toHookRep)
import Mosaico.Paper (mosaicoPaper)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

-- | Represents a frontpage
data Frontpage = List ListFrontpageProps | Prerendered PrerenderedFrontpageProps

type ListFrontpageProps =
  { categoryLabel :: Maybe String
  , content :: Maybe (Array ArticleStub)
  , onArticleClick :: ArticleStub -> EventHandler
  , onTagClick :: Tag -> EventHandler
  }

type PrerenderedFrontpageProps =
  { content :: Maybe String
  , hooks   :: Array Hook
  , onClick :: EventHandler
  }

render :: Frontpage -> JSX
render (List props) =
  let category = fromMaybe mempty props.categoryLabel
  in DOM.h1_ [ DOM.text category] <>
      genericRender (\list -> map renderListArticle list) mempty props.content
      where
        renderListArticle :: ArticleStub -> JSX
        renderListArticle a =
          DOM.div
            { className: "mosaico--list-article list-article--default"
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
                        [ -- TODO: paper specific fallback img
                        let imgSrc = maybe (fallbackImage mosaicoPaper) _.url (a.listImage <|> a.mainImage)
                        in DOM.a
                            { href: "/artikel/" <> a.uuid
                            , className: "list-article-image"
                            , children: [ DOM.img { src: imgSrc <> "&function=hardcrop&width=200&height=200&q=90" } ]
                            }
                        ,  DOM.div
                              { className: "list-article-liftup"
                              , children:
                                  [ foldMap
                                      (\tag ->
                                          DOM.a
                                            { className: "mosaico-article__tag"
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
                                              { className: "premium-badge"
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
render (Prerendered props@{ hooks }) = genericRender
  
  (\content -> [ HtmlRenderer.render
                   { content
                   , hooks: Just $ toHookRep <$> hooks
                   }
                ]
  )
  props.onClick
  props.content

genericRender :: forall a. (a -> Array JSX) -> EventHandler -> Maybe a -> JSX
genericRender f onClick content = DOM.div
  { className: "mosaico--article-list"
  , children: maybe [loadingSpinner] f content
  , onClick
  }
