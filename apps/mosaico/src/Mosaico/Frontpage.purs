module Mosaico.Frontpage
  ( Frontpage(..)
  , ListFrontpageProps(..)
  , PrerenderedFrontpageProps(..)
  , render
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (head, null)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (un, unwrap)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import KSF.Helpers (formatArticleTime)
import KSF.HtmlRenderer (render) as HtmlRenderer
import KSF.Spinner (loadingSpinner)
import Lettera.Models (ArticleStub, Tag(..), tagToURIComponent)
import Mosaico.BreakingNews as BreakingNews
import Mosaico.FallbackImage (fallbackImage)
import Mosaico.Frontpage.Models (Hook, toHookRep)
import Mosaico.Paper (mosaicoPaper)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

-- | Represents a frontpage
data Frontpage = List ListFrontpageProps | Prerendered PrerenderedFrontpageProps

type ListFrontpageProps =
  { label :: Maybe String
  , content :: Maybe (Array ArticleStub)
  , onArticleClick :: ArticleStub -> EventHandler
  , onTagClick :: Tag -> EventHandler
  }

type PrerenderedFrontpageProps =
  { content :: Maybe String
  , breakingNews :: String
  , hooks   :: Array Hook
  , onClick :: EventHandler
  }

render :: Frontpage -> JSX
render (List props) =
    DOM.div
        { className: "mx-4 mosaico--article-list md:mx-0"
        , children:
          [ maybeLabel props.label
          , genericRender (\list -> map renderListArticle list) mempty props.content
          ]
        }
      where
        -- Only OC images use this
        addCrop url =
          if contains (Pattern "smooth-storage") url then url
          else url <> "&function=hardcrop&width=200&height=200&q=90"
        tagLink a = foldMap (\tag ->
                                DOM.a
                                  { className: "relative z-20 mb-1 mosaico-article__tag"
                                  , onClick: props.onTagClick tag
                                  , href: "/tagg/" <> tagToURIComponent tag
                                  , children: [ DOM.text $ un Tag tag ]
                                  }
                            ) $ head a.tags

        articleTitle a = [ DOM.h3
                             { className: "text-2xl leading-tight text-gray-900 font-duplexserif"
                             , children: [ DOM.text $ fromMaybe a.title a.listTitle ]
                             }
                         , DOM.span
                             { className: "block mb-1 font-roboto"
                             , children: [ DOM.text $ foldMap (formatArticleTime <<< unwrap) a.publishingTime ]
                             }
                         , guard a.premium $
                           DOM.div
                             { className: "mosaico-article__meta"
                             , children:
                                 [ DOM.div
                                     { className: "premium-badge"
                                     , children: [ DOM.text "premium" ]
                                     }
                                 ]
                             }
                         ]

        listArticleImage a =
          let img = a.listImage <|> a.mainImage
              src = maybe (fallbackImage mosaicoPaper) (addCrop <<< _.tinyThumb) img
              alt = fold $ _.caption =<< img
          in  DOM.img
                { src
                , alt
                , className: "w-20 h-full md:w-28"
                }

        renderListArticle :: ArticleStub -> JSX
        renderListArticle a =
          DOM.article
            { className: "flex relative justify-between mb-5 cursor-pointer"
            , _data: Object.fromFoldable $
              -- Known bug, exclude from tests
              if null a.tags || Just true == (contains (Pattern ":") <<< un Tag <$> head a.tags) then []
              else [ Tuple "premium" $ if a.premium then "1" else "0"
                    , Tuple "uuid" $ a.uuid
                    ]
            , children:
                [ DOM.div
                    { className: "mr-4 break-words"
                    , children:
                        [ tagLink a
                        , DOM.a
                            { href: "/artikel/" <> a.uuid
                            , onClick: props.onArticleClick a
                            , children: articleTitle a
                            -- these ::after styles will be on top of everything except the tag links,
                            -- making everything clickable
                            , className: "no-underline after:w-full after:h-full after:absolute after:top-0 after:left-0 after:z-10"
                            }
                        ]
                    }
                , listArticleImage a
                ]
            }

render (Prerendered props@{ hooks }) = genericRender
  (\content -> [
    BreakingNews.render {content: props.breakingNews}
    , HtmlRenderer.render
                   { content
                   , hooks: Just $ toHookRep <$> hooks
                   }
                ]
  )
  props.onClick
  props.content

maybeLabel :: Maybe String -> JSX
maybeLabel categoryLabel =
  case categoryLabel of
    Just label -> DOM.h2
                    { className: "inline-block mb-8 text-3xl font-bold leading-none border-b-2 font-roboto border-brand"
                    , children: [ DOM.text label ]
                    }
    _          -> mempty

genericRender :: forall a. (a -> Array JSX) -> EventHandler -> Maybe a -> JSX
genericRender f onClick content = DOM.div
  { className: "mosaico-main"
  , children: maybe [loadingSpinner] f content
  , onClick
  }
