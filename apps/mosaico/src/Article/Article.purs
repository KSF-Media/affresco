module Mosaico.Article where

import Prelude

import Bottega.Models.Order (OrderSource(..))
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array (head, insertAt, length, null, snoc, take, (!!))
import Data.Either (Either(..), either, hush)
import Data.Foldable (fold, foldMap)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (un, unwrap)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import KSF.Helpers (formatArticleTime)
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import KSF.Vetrina as Vetrina
import KSF.Vetrina.Products.Premium (hblPremium, vnPremium, onPremium)
import Lettera.Models (Article, ArticleStub, ArticleType(..), BodyElement(..), FullArticle, Image, LocalDateTime(..), MosaicoArticleType(..), PartStartInfo, Tag(..), tagToURIComponent)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.Article.Box (box)
import Mosaico.Article.Image as Image
import Mosaico.Eval (ScriptTag(..), evalExternalScripts)
import Mosaico.FallbackImage (fallbackImage)
import Mosaico.Frontpage (Frontpage(..), render) as Frontpage
import Mosaico.LatestList as LatestList
import Mosaico.Share as Share
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Basic.Hooks (Component)
import React.Basic.Events (EventHandler)

type RenderElement =
  { el :: BodyElement
  , reviewRow :: Boolean
  , partLabel :: Maybe String
  }

type GroupedElements =
  { els :: Array BodyElement
    -- Set true for the inner content of review block
  , reviewRow :: Boolean
  , partStart :: Maybe PartStartInfo
  }

isPremium :: Either ArticleStub FullArticle -> Boolean
isPremium = either _.premium _.article.premium

getTags :: Either ArticleStub FullArticle -> Array Tag
getTags = either _.tags _.article.tags

getTitle :: Either ArticleStub FullArticle -> String
getTitle = either _.title _.article.title

getMainImage :: Either ArticleStub FullArticle -> Maybe Image
getMainImage = either _.mainImage _.article.mainImage

getPreamble :: Either ArticleStub FullArticle -> Array String
getPreamble = either _.preamble _.article.preamble

getBody :: Either ArticleStub FullArticle -> Array BodyElement
getBody = either (const mempty) _.article.body

getRemoveAds :: Either ArticleStub FullArticle -> Boolean
getRemoveAds = either _.removeAds _.article.removeAds

getShareUrl :: Either ArticleStub FullArticle -> Maybe String
getShareUrl = either _.shareUrl _.article.shareUrl

type Props =
  { paper :: Paper
  , article :: Either ArticleStub FullArticle
  , onLogin :: EventHandler
  , onPaywallEvent :: Effect Unit
  , onTagClick :: Tag -> EventHandler
  , onArticleClick :: ArticleStub -> EventHandler
  , user :: Maybe (Maybe User)
  , mostReadArticles :: Array ArticleStub
  , latestArticles :: Array ArticleStub
  , advertorial :: Maybe ArticleStub
  }

evalEmbeds :: Article -> Effect Unit
evalEmbeds = evalExternalScripts <<< map ScriptTag <<< map unwrap <<< fold <<< _.externalScripts

component :: Component Props
component = do
  imageComponent <- Image.component
  React.component "Article" $ \props -> React.do
    pure $ render imageComponent props

render :: (Image.Props -> JSX) -> Props -> JSX
render imageComponent props =
    let title = getTitle props.article
        tags = getTags props.article
        mainImage = getMainImage props.article
        body = getBody props.article
        bodyWithoutAd = map (renderGroup (Just props.paper) imageComponent (Just props.onArticleClick)) <<< groupParts $ body
        bodyWithAd = map (renderGroup (Just props.paper) imageComponent (Just props.onArticleClick))
          <<< groupParts
          <<< insertAdsIntoBodyText "mosaico-ad__bigbox1" "mosaico-ad__bigbox2" $ body
        advertorial = foldMap renderAdvertorialTeaser props.advertorial
        mostRead = foldMap renderMostReadArticles $
          if null props.mostReadArticles then Nothing else Just $ take 5 props.mostReadArticles
        shareUrl = getShareUrl props.article

    in DOM.article
      { className: "mosaico-article"
      , children:
          [ DOM.header_
            [ DOM.h1
                { className: "mosaico-article__headline"
                , children: [ DOM.text title ]
                }
            , DOM.section
                { className: "mosaico-article__preamble"
                , children: map (DOM.p_ <<< pure <<< DOM.text) $ getPreamble props.article
                }
            -- We don't want to be able to share error articles
            , guard (maybe true ((_ /= ErrorArticle) <<< _.articleType) $ hush props.article)
                DOM.section
                  { className: "mosaico-article__tag-n-share"
                  , children:
                      [ DOM.div
                          { className: "mosaico-article__tag-n-premium"
                          , children:
                              [ foldMap renderTag $ head tags
                              , guard (isPremium props.article) $ DOM.div
                                  { className: "premium-badge"
                                  , children: [ DOM.span_ [ DOM.text "Premium" ]]
                                  }
                              ]
                          }
                      , Share.articleShareButtons title shareUrl
                      ]
                  }
            ]
          , foldMap
              (\image -> imageComponent
                { clickable: true
                , main: true
                , params: Just "&width=960&height=540&q=90"
                , image
                , fullWidth: false
                })
              mainImage
          , DOM.div
              { className: "mosaico-article__main"
              , children:
                    [ foldMap (renderMetabyline <<< _.article) $ hush props.article
                    , DOM.div
                        { className: "mosaico-article__body "
                        , children: case _.articleType <$> props.article of
                          Right PreviewArticle ->
                            bodyWithAd
                            `snoc` paywallFade
                            `snoc` (if isNothing props.user then loadingSpinner else vetrina)
                            `snoc` advertorial
                            `snoc` mostRead
                          Right DraftArticle ->
                            bodyWithoutAd
                          Right FullArticle ->
                            bodyWithAd
                            `snoc` advertorial
                            `snoc` mostRead
                          Left _ -> [ loadingSpinner ]
                          _ -> mempty
                        }
                    , DOM.div
                        { className: "mosaico-article__aside"
                        , children:
                          [ Mosaico.ad { contentUnit: "mosaico-ad__box" }
                          , LatestList.render
                                     { latestArticles: props.latestArticles
                                     , onClickHandler: props.onArticleClick
                                     }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box1" }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box2" }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box3" }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box4" }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box5" }
                          ]
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
                    (if null article.authors && article.articleType == Opinion
                     then [ renderOpinionType article.articleTypeDetails ]
                     else map
                        (\author -> DOM.div
                          { className: "mosaico-article__author"
                          , children: [ guard (article.articleType == Opinion) $
                                        renderOpinionType article.articleTypeDetails
                                      , DOM.span_ [ DOM.text author.byline ]
                                      , foldMap
                                        (\authorEmail -> DOM.span
                                                        { className: "mosaico-article__author-email"
                                                        , children: [ DOM.text authorEmail ]
                                                        }
                                        )
                                        author.email
                                      ]
                          })
                        article.authors
                    )
                    <> [foldMap
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

    renderOpinionType detail =
      foldMap (\opiniontype -> DOM.span
                              { className: "mosaico-article__opinion-type"
                              , children: [ DOM.text opiniontype ]
                              }) $ _.title <$> detail

    renderTag tag =
      DOM.a
        { className: "mosaico-article__tag"
        , children: [ DOM.text $ (un Tag) tag ]
        , href: "/tagg/" <> tagToURIComponent tag
        , onClick: props.onTagClick tag
        }

    paywallFade =
        DOM.div { className: "mosaico--article-fading-body" }

    vetrina =
      Vetrina.vetrina
        { onClose: Just props.onPaywallEvent
        , onLogin: props.onLogin
        , user: join props.user
        , products: Right case props.paper of
            HBL -> [ hblPremium ]
            ON -> [ onPremium ]
            VN -> [ vnPremium ]
            _ -> []
        , unexpectedError: mempty
        , headline: Just
          $ DOM.div_
              [ DOM.text $ "Läs " <> paperName <> " digitalt för "
              , DOM.span { className: "vetrina--price-headline", children: [ DOM.text "endast 1€" ] }
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

    renderMostReadArticles articles =
      DOM.div
        { className: "mosaico-article__mostread--header"
        , children: [ DOM.text "ANDRA LÄSER" ]
        } <>
      (Frontpage.render $ Frontpage.List
        { label: mempty
        , content: Just articles
        , onArticleClick: props.onArticleClick
        , onTagClick: props.onTagClick
        })

    renderAdvertorialTeaser article =
      let
        imgSrc =
          maybe (fallbackImage props.paper)
            _.thumb
            (article.listImage <|> article.mainImage)
      in
        DOM.a
          { className: "mosaico--list-article-advertorial"
          , href: "/artikel/" <> article.uuid
          , onClick: props.onArticleClick article
          , children:
              [ DOM.img
                  { className: "mosaico--list-article-advertorial__image"
                  , src: imgSrc
                  }
              , DOM.h2
                  { className: "mosaico--list-article-advertorial__title"
                  , children: [ DOM.text $ fromMaybe article.title article.listTitle ]
                  }
              ]
          }


    insertAdsIntoBodyText :: String -> String -> Array BodyElement -> Array BodyElement
    insertAdsIntoBodyText contentUnit1 contentUnit2 body =
      let ad1 = Ad contentUnit1
          ad2 = Ad contentUnit2
      in if elements > 15
        then fromMaybe body $ do
          bodyWithAd <- insertAt (findAdSpace body $ elements/3) ad1 body
          insertAt (findAdSpace bodyWithAd $ 2 * elements/3) ad2 bodyWithAd
        else if elements > 6
          then fromMaybe body $ insertAt (findAdSpace body $ elements/2) ad1 body
          else body `snoc` ad1
      where
        elements = length body
        findAdSpace :: Array BodyElement -> Int -> Int
        findAdSpace body' i
          | i > elements = elements
          | Just (Html _) <- body' !! (i-1)
          , Just (Html _) <- body' !! (i)
          = i
          | otherwise = findAdSpace body' (i+1)

-- TODO: maybe we don't want to deal at all with the error cases
-- and we want to throw them away?
renderElement :: Maybe Paper -> (Image.Props -> JSX) -> Maybe (ArticleStub -> EventHandler) -> RenderElement -> JSX
renderElement paper imageComponent onArticleClick el = case el.el of
  Html content ->
    -- Can't place div's or blockquotes under p's, so place them under div.
    -- This is usually case with embeds
    let domFn = if isDiv content || isBlockquote content then DOM.div else DOM.p
        taggedContent = case el.partLabel of
          Nothing -> content
          Just label -> "<b>" <> label <> "</b> " <> content
    in domFn
       { dangerouslySetInnerHTML: { __html: content }
       , className: block <> " " <> block <> "__html"
       }
  Headline str -> DOM.h4
    { className: block <> " " <> block <> "__subheadline"
    , children: [ DOM.text str ]
    }
  Image image -> imageComponent
      { clickable: true
      , main: false
      , params: Just "&width=640&q=90"
      , image
      , fullWidth: false
      }
  Box boxData ->
    DOM.div
      { className: block <> " " <> block <> "__factbox"
      , children:
          [ box
              { headline: boxData.headline
              , title: boxData.title
              , content: boxData.content
              , paper: paper
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
  PartStart _ -> mempty
  PartEnd _ -> mempty
  Ad contentUnit ->
      Mosaico.ad {
        contentUnit
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
            , onClick: foldMap (\f -> f article) onArticleClick
            }
        ]

renderGroup :: Maybe Paper -> (Image.Props -> JSX) -> Maybe (ArticleStub -> EventHandler) -> GroupedElements -> JSX
renderGroup paper imageComponent onArticleClick { els, partStart, reviewRow } =
  case _.type =<< partStart of
    Just "Recension" ->
      DOM.div
        { className: "article-element__review-info"
        , children: [ renderGroup paper imageComponent onArticleClick
                      { els, partStart: Nothing, reviewRow: true }
                    ]
        }
    _ ->
      (_ <> (foldMap renderAuthor <<< join <<< Array.fromFoldable $ _.authors <$> partStart)) <<<
      foldMap (renderElement paper imageComponent onArticleClick) <<< maybeAddPartLabel $
      map (\el -> {el, partLabel: Nothing, reviewRow}) els
  where
    maybeAddPartLabel :: Array RenderElement -> Array RenderElement
    maybeAddPartLabel = case _.type =<< partStart of
      label@(Just _) -> fromMaybe mempty <<< Array.modifyAt 0 (_ { partLabel = label })
      _              -> identity

    renderAuthor { byline, info } =
      DOM.p
        { className: "article-element article-element__author"
        , children:
            [ DOM.strong_ [ DOM.text $ byline <> foldMap (const ",") info ]
            , foldMap (DOM.text <<< (" " <> _)) info
            ]
        }

mkShareIcon :: String -> JSX
mkShareIcon someName =
  DOM.li_
    [ DOM.a
        { href: "#"
        , children: [ DOM.span {} ]
        , className: "mosaico-article__some--" <> someName
        }
    ]

groupParts :: Array BodyElement -> Array GroupedElements
groupParts = Array.reverse <<<
       map (\part -> { els: Array.reverse $ Array.fromFoldable part.els
                     , reviewRow: false
                     , partStart: part.partStart }) <<<
       Array.fromFoldable <<< Array.foldl (flip f) mempty
  where
    f (PartStart info) = List.Cons { els: mempty, partStart: Just info, done: false }
    f (PartEnd _) = fromMaybe mempty <<< List.modifyAt 0 (\x -> x { done = true })
    f x = \xs -> case List.head xs of
      Just { done: false } -> fromMaybe mempty $ List.modifyAt 0
           (\part@{ els } -> part { els = List.Cons x els }) xs
      _ -> List.Cons { els: List.singleton x, partStart: Nothing, done: true } xs
