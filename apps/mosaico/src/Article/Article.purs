module Mosaico.Article where

import Prelude

import Bottega.Models.Order (OrderSource(..))
import Control.Alt ((<|>))
import Data.Array (head, insertAt, length, null, snoc, take, (!!))
import Data.Either (Either(..), either, hush)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (un, unwrap)
import Data.Set as Set
import Data.String (toUpper)
import Effect (Effect)
import KSF.Helpers (formatArticleTime)
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import KSF.Vetrina as Vetrina
import KSF.Vetrina.Products.Premium (hblPremium, vnPremium, onPremium)
import Lettera.Models (Article, ArticleStub, ArticleType(..), BodyElement(..), FullArticle, Image, MosaicoArticleType(..), Tag(..), tagToURIComponent)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.Article.Box as Box
import Mosaico.BreakingNews as BreakingNews
import Mosaico.Article.Image as Image
import Mosaico.Eval (ScriptTag(..), evalExternalScripts)
import Mosaico.FallbackImage (fallbackImage)
import Mosaico.Frontpage (Frontpage(..), render) as Frontpage
import Mosaico.LatestList as LatestList
import Mosaico.Share as Share
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component)

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
  , breakingNews :: String
  }

evalEmbeds :: Article -> Effect Unit
evalEmbeds = evalExternalScripts <<< map ScriptTag <<< map unwrap <<< fold <<< _.externalScripts

component :: Component Props
component = do
  imageComponent <- Image.component
  boxComponent <- Box.component
  React.component "Article" $ \props -> React.do
    pure $ render imageComponent boxComponent props

render :: (Image.Props -> JSX) -> (Box.Props -> JSX) -> Props -> JSX
render imageComponent boxComponent props =
    let title = getTitle props.article
        tags = getTags props.article
        mainImage = getMainImage props.article
        body = getBody props.article
        bodyWithoutAd = map (renderElement imageComponent boxComponent (Just props.onArticleClick)) body
        bodyWithAd = 
          [ DOM.section
            { className: "article-content"
            , children: map (renderElement imageComponent boxComponent (Just props.onArticleClick))
                <<< insertAdsIntoBodyText "mosaico-ad__bigbox1" "mosaico-ad__bigbox2" $ body
            }
          ]
        advertorial = foldMap renderAdvertorialTeaser props.advertorial
        mostRead = foldMap renderMostReadArticles $
          if null props.mostReadArticles then Nothing else Just $ take 5 props.mostReadArticles
        shareUrl = getShareUrl props.article

    in DOM.article
      { className: "mosaico-article"
      , children:
          [ DOM.div {className: "[grid-area:breaking] lg:mx-24", children: [BreakingNews.render { content: props.breakingNews }]}
          , DOM.header_
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
                        { className: "mosaico-article__body"
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
                          [ Mosaico.ad { contentUnit: "mosaico-ad__box", inBody: false }
                          , LatestList.render
                                     { latestArticles: props.latestArticles
                                     , onClickHandler: props.onArticleClick
                                     }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box1", inBody: false }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box2", inBody: false }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box3", inBody: false }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box4", inBody: false }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box5", inBody: false }
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
                    map
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
                    <> [foldMap
                        (\publishingTime -> DOM.div
                          { className: "mosaico-article__timestamps"
                          , children:
                              [ DOM.span_ [ DOM.text publishingTime ]
                              , foldMap
                                (\updateTime -> DOM.span_
                                  [ DOM.text $ " UPPDATERAD " <> updateTime]
                                )
                                ((\x -> guard (x /= publishingTime) $ Just x) =<<
                                 formatArticleTime <<< unwrap <$> article.updateTime)
                              ]
                          })
                        (formatArticleTime <<< unwrap <$> article.publishingTime)
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
        , children: [ DOM.h2_ [DOM.text "ANDRA LÄSER" ]]
        } <>
      (Frontpage.render $ Frontpage.List
        { label: mempty
        , content: Just articles
        , onArticleClick: props.onArticleClick
        , onTagClick: props.onTagClick
        })

    renderAdvertorialTeaser :: ArticleStub -> JSX
    renderAdvertorialTeaser article =
      let img = article.listImage <|> article.mainImage
          imgSrc = maybe (fallbackImage props.paper) _.thumb img
          alt = fromMaybe "" $ _.caption =<< img
      in
        DOM.a
          { className: "block p-3 text-black no-underline bg-advertorial"
          , href: "/artikel/" <> article.uuid
          , onClick: props.onArticleClick article
          , children:
              [ DOM.div
                  { className: "pt-1 pb-2 text-xs font-bold font-duplexserif"
                  , children: case article.articleTypeDetails of
                        Just { title: "companyName", description: Just company } ->
                          [ DOM.span
                              { className: "mr-1 text-gray-500 font-roboto"
                              , children: [ DOM.text "ANNONS: " ]
                              }
                          , DOM.span
                            { className: "mr-1 text-black font-duplexserif"
                            , children: [ DOM.text $ toUpper company ]
                            }
                          ]
                        _ ->
                          [ DOM.span
                              { className: "mr-1 text-gray-500 font-roboto"
                              , children: [ DOM.text "ANNONS" ]
                              }
                          ]
                  }
              , DOM.img
                  { className: "w-auto max-w-full h-auto max-h-96"
                  , src: imgSrc
                  , alt
                  }
              , DOM.h2
                  { className: "mt-3 text-3xl font-semibold font-robotoslab"
                  , children: [ DOM.text $ fromMaybe article.title article.listTitle ]
                  }
              ]
          }


    insertAdsIntoBodyText :: String -> String -> Array BodyElement -> Array BodyElement
    insertAdsIntoBodyText cu1 cu2 body =
      if elements > 15
        then fromMaybe body $ do
          bodyWithAd <- insertAt (findAdSpace body $ elements/3) (Ad { contentUnit: cu1, inBody: true }) body
          insertAt (findAdSpace bodyWithAd $ 2 * elements/3) (Ad { contentUnit: cu2, inBody: true }) bodyWithAd
        else if elements > 6
          then fromMaybe body $ insertAt (findAdSpace body $ elements/2) (Ad { contentUnit: cu1, inBody: true }) body
          else body `snoc` (Ad { contentUnit: cu1, inBody: false })
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
renderElement :: (Image.Props -> JSX) -> (Box.Props -> JSX) -> Maybe (ArticleStub -> EventHandler) -> BodyElement -> JSX
renderElement imageComponent boxComponent onArticleClick el =  case el of
  -- Can't place div's or blockquotes under p's, so place them under div.
  -- This is usually case with embeds
  Html content -> DOM.div
    { dangerouslySetInnerHTML: { __html: content }
    , className: block <> " " <> block <> "__html"
    }
  Headline str -> DOM.h2
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
      { className: block <> " " <> block <> "__" <> boxCSSType
      , children:
          [ boxComponent
              { headline: boxData.headline
              , title: boxData.title
              , content: boxData.content
              , expanded: boxData.type == "review" || Box.autoExpand boxData.content
              }
          ]
      }
    where
      boxCSSType = case boxData.type of
        "review" -> "reviewbox"
        _        -> "factbox"
  Footnote footnote -> DOM.p
      { className: block <> " " <> block <> "__footnote"
      , dangerouslySetInnerHTML: { __html: footnote }
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
  Ad { contentUnit, inBody} ->
      Mosaico.ad {
        contentUnit,
        inBody
      }
  where
    block = "article-element"
    renderRelatedArticle article =
      DOM.li_
        [ DOM.a
            { href: "/artikel/" <> article.uuid
            , children: [ DOM.text article.title ]
            , onClick: foldMap (\f -> f article) onArticleClick
            }
        ]
