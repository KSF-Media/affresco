module Mosaico.Article where

import Prelude

import Bottega.Models.Order (OrderSource(..))
import Data.Array (cons, head, snoc)
import Data.Either (Either(..), hush)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (un, unwrap)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import KSF.Helpers (formatArticleTime)
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import KSF.User (User)
import KSF.Vetrina as Vetrina
import KSF.Vetrina.Products.Premium (hblPremium, vnPremium, onPremium)
import Lettera.Models (Article, ArticleStub, BodyElement(..), FullArticle(..), Image, LocalDateTime(..), Tag(..), fromFullArticle, isErrorArticle, tagToURIComponent)
import Mosaico.Ad as Ad
import Mosaico.Article.Box (box)
import Mosaico.Article.Image (articleMainImage, articleImage)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

foreign import evalExternalScriptsImpl :: EffectFn1 (Array String) Unit
evalExternalScripts :: Array String -> Effect Unit
evalExternalScripts = runEffectFn1 evalExternalScriptsImpl

isPremium :: Either ArticleStub FullArticle -> Boolean
isPremium (Left articleStub) = articleStub.premium
isPremium (Right fullArticle) = _.premium $ fromFullArticle fullArticle

getTags :: Either ArticleStub FullArticle -> Array Tag
getTags (Left articleStub) = articleStub.tags
getTags (Right fullArticle) = _.tags $ fromFullArticle fullArticle

getTitle :: Either ArticleStub FullArticle -> String
getTitle (Left articleStub) = articleStub.title
getTitle (Right fullArticle) = _.title $ fromFullArticle fullArticle

getMainImage :: Either ArticleStub FullArticle -> Maybe Image
getMainImage (Left articleStub) = articleStub.mainImage
getMainImage (Right fullArticle) = _.mainImage $ fromFullArticle fullArticle

getPreamble :: Either ArticleStub FullArticle -> Maybe String
getPreamble (Left articleStub) = articleStub.preamble
getPreamble (Right fullArticle) = _.preamble $ fromFullArticle fullArticle

getBody :: Either ArticleStub FullArticle -> Array BodyElement
getBody (Left _articleStub) = mempty
getBody (Right fullArticle) = _.body $ fromFullArticle fullArticle

type Props =
  { paper :: Paper
  , article :: Either ArticleStub FullArticle
  , onLogin :: Effect Unit
  , onPaywallEvent :: Effect Unit
  , onTagClick :: Tag -> EventHandler
  , onArticleClick :: ArticleStub -> EventHandler
  , user :: Maybe User
  }

evalEmbeds :: Article -> Effect Unit
evalEmbeds = evalExternalScripts <<< map unwrap <<< fold <<< _.externalScripts

render :: Props -> JSX
render props =
    let title = getTitle props.article
        tags = getTags props.article
        mainImage = getMainImage props.article
        bodyWithAd = Ad.insertIntoBody adBox $ map renderElement $ getBody props.article
        draftHeader = case props.article of
          Right (DraftArticle _) ->
            DOM.div
              { className: "mosaico-article--draft"
              , children: [ DOM.text "Förslag" ]
              }
          _ -> mempty
    in DOM.article
      { className: "mosaico-article"
      , children:
          [ DOM.header_
            [ draftHeader
            , DOM.h1
                { className: "mosaico-article__headline"
                , children: [ DOM.text title ]
                }
            , DOM.section
                { className: "mosaico-article__preamble"
                , children: [ DOM.text $ fromMaybe mempty $ getPreamble props.article ]
                }
            -- We don't want to be able to share error articles
            , guard (maybe true (not <<< isErrorArticle) $ hush props.article)
                DOM.section
                  { className: "mosaico-article__tag-n-share"
                  , children:
                      [ DOM.div
                          { className: "mosaico-article__tag-n-premium"
                          , children:
                              [ foldMap renderTag $ head tags
                              , guard (isPremium props.article) $ DOM.div
                                  { className: "premium-badge background-" <> Paper.cssName props.paper
                                  , children: [ DOM.text "Premium"]
                                  }
                              ]
                          }
                      , DOM.ul
                          { className: "mosaico-article__some"
                          , children: map mkShareIcon [ "facebook", "twitter", "linkedin", "whatsapp", "mail" ]
                          }
                      ]
                  }
            ]
          , articleMainImage
              { clickable: true
              , params: Just "&width=960&height=540&q=90"
              , image: fold mainImage
              }
          , DOM.div
              { className: "mosaico-article__main"
              , children:
                    [ foldMap (renderMetabyline <<< fromFullArticle) $ hush props.article
                    , DOM.div
                        { className: "mosaico-article__body "
                        , children: case props.article of
                          (Right (PreviewArticle _previewArticle)) ->
                            paywallFade
                            `cons` bodyWithAd
                            `snoc` vetrina
                          (Right (DraftArticle _draftArticle)) ->
                            map renderElement $ getBody props.article
                          (Right (FullArticle _fullArticle)) ->
                            bodyWithAd
                          _ -> mempty
                        }
                    , DOM.div
                        { className: "mosaico-article__aside"
                        , children: []
                        }
                    ]
              }
          ]
    }
  where
    renderMetabyline :: Article -> JSX
    renderMetabyline article =
      DOM.div
        { className: "mosaico-article__metabyline"
        , children:
            [ DOM.div
                { className: "mosaico-article__authors-and-timestamps"
                , children:
                    [ foldMap
                        (\authorName -> DOM.div
                          { className: "mosaico-article__author"
                          , children: [ DOM.text authorName]
                          })
                        (_.byline <$> article.authors)
                    , foldMap
                        (\(LocalDateTime publishingTime) -> DOM.div
                          { className: "mosaico-article__timestamps"
                          , children:
                              [ DOM.span_ [ DOM.text $ formatArticleTime publishingTime]
                              , foldMap
                                (\(LocalDateTime updateTime) -> DOM.span_
                                  [ DOM.text $ " UPPDATERAD " <> formatArticleTime updateTime]
                                )
                                article.updateTime
                              ]
                          })
                        article.publishingTime
                    ]
                }
            ]
        }

    renderTag tag =
      DOM.a
        { className: "mosaico-article__tag color-" <> Paper.cssName props.paper
        , children: [ DOM.text $ (un Tag) tag ]
        , href: "/tagg/" <> tagToURIComponent tag
        , onClick: props.onTagClick tag
        }

    mkShareIcon someName =
      DOM.li_
        [ DOM.a
            { href: "#"
            , children: [ DOM.span {} ]
            , className: "mosaico-article__some--" <> someName
            }
        ]

    adBox =
        Ad.ad { contentUnit: "JATTEBOX" }

    paywallFade =
        DOM.div { className: "mosaico--article-fading-body" }

    vetrina =
      Vetrina.vetrina
        { onClose: Just props.onPaywallEvent
        , onLogin: props.onLogin
        , user: props.user
        , products: Right case props.paper of
            HBL -> [ hblPremium ]
            ON -> [ onPremium ]
            VN -> [ vnPremium ]
            _ -> []
        , unexpectedError: mempty
        , headline: Just
          $ DOM.div_
              [ DOM.text $ "Läs " <> paperName <> " digitalt för "
              , DOM.span { className: "vetrina--price-headline", children: [ DOM.text "Endast 1€" ] }
              ]
        , paper: Just props.paper
        , paymentMethods: []
        , customNewPurchase: Nothing
        , subscriptionExists: mempty
        , loadingContainer: Nothing
        , accessEntitlements: Set.fromFoldable case props.paper of
            HBL -> ["hbl-365", "hbl-web"]
            ON -> ["on-365", "on-web"]
            VN -> ["vn-365", "vn-web"]
            _ -> []
        , orderSource: PaywallSource
        , askAccountAlways: false
        }

    paperName = case props.paper of
      HBL -> "HBL"
      p -> Paper.paperName p

    -- TODO: maybe we don't want to deal at all with the error cases
    -- and we want to throw them away?
    renderElement :: BodyElement -> JSX
    renderElement el = case el of
      Html content ->
        -- Can't place div's or blockquotes under p's, so place them under div.
        -- This is usually case with embeds
        let domFn = if isDiv content || isBlockquote content then DOM.div else DOM.p
        in domFn
           { dangerouslySetInnerHTML: { __html: content }
           , className: block <> " " <> block <> "__html"
           }
      Headline str -> DOM.h4
        { className: block <> " " <> block <> "__subheadline"
        , children: [ DOM.text str ]
        }
      Image img -> articleImage
          { clickable: true
          , params: Just "&width=640&q=90"
          , image: img
          }
      Box boxData ->
        DOM.div
          { className: block <> " " <> block <> "__factbox"
          , children:
              [ box
                  { headline: boxData.headline
                  , title: boxData.title
                  , content: boxData.content
                  , paper: props.paper
                  }
              ]
          }
      Footnote footnote -> DOM.p
          { className: block <> " " <> block <> "__footnote"
          , children: [ DOM.text footnote ]
          }
      Quote { body, author } -> DOM.figure
          { className: block <> " " <> block <> "__quote"
          , children:
              [ DOM.blockquote_ [ DOM.text body ]
              , foldMap (DOM.figcaption_ <<< pure <<< DOM.text) author
              ]
          }
      Question question -> DOM.p
          { className: block <> " " <> block <> "__question"
          , children: [ DOM.text question ]
          }
      Related related -> DOM.figure
          { className: block <> " " <> block <> "__related"
          , children:
              [ DOM.ul_ $ map renderRelatedArticle related
              ]
          }
      where
        block = "article-element"
        isDiv = isElem "<div"
        isBlockquote = isElem "<blockquote"
        -- Does the string start with wanted element
        isElem elemName elemString =
          Just 0 == String.indexOf (Pattern elemName) elemString
        renderRelatedArticle article =
          DOM.li_
            [ DOM.a
                { href: "/artikel/" <> article.uuid
                , children: [ DOM.text article.title ]
                , onClick: props.onArticleClick article
                }
            ]
