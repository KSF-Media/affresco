module Mosaico.Article where

import Prelude

import Bottega.Models.Order (OrderSource(..))
import Control.Alt ((<|>))
import Data.Array (cons, head, snoc)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep.RecordToSum as Record
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import KSF.Api.Package (CampaignLengthUnit(..))
import KSF.Helpers (formatArticleTime)
import KSF.Paper (Paper(..))
import KSF.User (User)
import KSF.Vetrina as Vetrina
import Lettera.Models (Article, ArticleStub, BodyElement(..), FullArticle(..), Image, LocalDateTime(..), fromFullArticle)
import Mosaico.Ad as Ad
import Mosaico.Article.Box (box)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useState, (/\))
import React.Basic.Hooks as React

foreign import evalExternalScriptsImpl :: EffectFn1 (Array String) Unit
evalExternalScripts :: Array String -> Effect Unit
evalExternalScripts = runEffectFn1 evalExternalScriptsImpl

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  , props :: Props
  }

type Props =
  { brand :: String
  , affArticle :: Aff FullArticle
  -- ^ `affArticle` is needed always, even if we get the `article`.
  --   In the case it's a premium article and the customer has no subscription,
  --   we need to load the article again after they make the purchase.
  --   You can think `affArticle` being the Lettera call to get the article.
  , article :: Maybe FullArticle
  , articleStub :: Maybe ArticleStub
  , onLogin :: Effect Unit
  , user :: Maybe User
  , uuid :: Maybe String
  }

type State =
  { body :: Array (Either String BodyElement)
  , article :: Maybe FullArticle
  , title :: String
  , mainImage :: Maybe Image
  , tags :: Array String
  , preamble :: Maybe String
  , premium :: Boolean
  }

articleComponent :: Component Props
articleComponent = do
  component "Article" \props -> React.do
    let article = fromFullArticle <$> props.article
    let initialState =
          { body: foldMap (map Record.toSum) $ _.body <$> article
          , article: props.article
          , title: fold $ _.title <$> article <|>
                          _.title <$> props.articleStub
          , mainImage: do
              articleStub <- props.articleStub
              articleStub.listImage
          , tags: fold $ _.tags <$> article <|>
                         _.tags <$> props.articleStub
          , preamble: fold $ _.preamble <$> article <|>
                             _.preamble <$> props.articleStub
          , premium: fromMaybe false $ _.premium <$> article <|>
                                       _.premium <$> props.articleStub
          }
    state /\ setState <- useState initialState

    useEffect props.uuid do
      loadArticle props.article setState props.affArticle
      pure mempty

    -- If user logs in / logs out, reload the article.
    -- NOTE: We simply compare the email attribute of `User`
    -- as not every attribute of `User` implements `Eq`
    -- TODO: Should probably be state.user, right?
    -- TODO: Actually, this should probably live some place else
    --       Leaving the code for reference until the whole thing is resolved
    -- useEffect (_.email <$> props.user) do
    --   loadArticle setState props.affArticle
    --   pure mempty

    pure $ render { state, setState, props }

loadArticle :: Maybe FullArticle -> ((State -> State) -> Effect Unit) -> Aff FullArticle -> Effect Unit
loadArticle articleProp setState affArticle = do
  case articleProp of
    Just a -> do
      -- We need to evaluate every external javascript Lettera gives us
      -- This is to get embeds working
      liftEffect $ evalExternalScripts $ map unwrap $ fold $ _.externalScripts $ fromFullArticle a
    Nothing -> Aff.launchAff_ do
      fullArticle <- affArticle
      let article = fromFullArticle fullArticle
      liftEffect do
        setState \s -> s
                    { article = Just fullArticle
                    , body = map Record.toSum article.body
                    , mainImage = article.mainImage
                    , title = article.title
                    , tags = article.tags
                    , preamble = article.preamble
                    , premium = article.premium
                    }
        evalExternalScripts $ map unwrap $ fold $ article.externalScripts

renderImage :: Image -> JSX
renderImage img =
  DOM.div
    { className: "mosaico-article__image"
    , children:
        [ DOM.img
            { src: img.url
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
    caption = fold img.caption
    byline  = fold img.byline

render :: Self -> JSX
render { props, state, setState } =
    let letteraArticle = map fromFullArticle state.article
        title = fromMaybe mempty $ map _.title letteraArticle <|> map _.title props.articleStub
        tags = fromMaybe mempty $ map _.tags letteraArticle <|> map _.tags props.articleStub
        mainImage = (_.mainImage =<< letteraArticle) <|> (_.listImage =<< props.articleStub)
        bodyWithAd = Ad.insertIntoBody adBox $ map renderElement state.body
        draftHeader = case state.article of
          Just (DraftArticle _) ->
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
                , children: [ DOM.text $ fromMaybe mempty state.preamble ]
                }
            , DOM.section
                { className: "mosaico-article__tag-n-share"
                , children:
                    [ DOM.div
                        { className: "mosaico-article__tag-n-premium"
                        , children:
                            [ DOM.div
                                { className: "mosaico-article__tag color-" <> props.brand
                                , children: [ DOM.text $ fromMaybe "" (head tags) ]
                                }
                            , guard state.premium $ DOM.div
                                { className: "premium-badge background-" <> props.brand
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
          , DOM.div
              { className: "mosaico-article__main-image"
              , children: [ foldMap renderImage mainImage ]
              }
          , DOM.div
              { className: "mosaico-article__main"
              , children:
                    [ foldMap (renderMetabyline <<< fromFullArticle) state.article
                    , DOM.div
                        { className: "mosaico-article__body "
                        , children: case state.article of
                          (Just (PreviewArticle _previewArticle)) ->
                            paywallFade
                            `cons` bodyWithAd
                            `snoc` vetrina
                          (Just (DraftArticle _draftArticle)) ->
                            map renderElement state.body
                          (Just (FullArticle _fullArticle)) ->
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
        { onClose: Just $ loadArticle props.article setState props.affArticle
        , onLogin: props.onLogin
        , products: Right [ hblPremium ]
        , unexpectedError: mempty
        , headline: Just
          $ DOM.div_
              [ DOM.text "Läs HBL digitalt för "
              , DOM.span { className: "vetrina--price-headline", children: [ DOM.text "Endast 1€" ] }
              ]
        , paper: Just HBL
        , paymentMethods: []
        , customNewPurchase: Nothing
        , subscriptionExists: mempty
        , loadingContainer: Nothing
        , accessEntitlements: Set.fromFoldable ["hbl-365", "hbl-web"]
        , orderSource: PaywallSource
        }
      where
        hblPremium =
          { id: "HBL WEBB"
          , name: "Hufvudstadsbladet Premium"
          , priceCents: 999
          , description:
              DOM.div_
                [ DOM.text "Kvalitetsjournalistik när, var och hur du vill."
                , DOM.br {}
                , DOM.text "Läs Hufvudstadsbladet för 1€ i en månad, därefter 9,99€ / månad tills vidare. Avsluta när du vill."
                ]
          , descriptionPurchaseCompleted: DOM.text "Du kan nu läsa Premiumartiklar på HBL.fi."
          , campaign: Just
              { no: 4701
              , id: "1MÅN1 EURO"
              , name: "FÖRSTA MÅNADEN FÖR 1 EURO"
              , length: 1
              , lengthUnit: Month
              , priceEur: 1.0
              }
          , contents:
              [ { title: "Premium"
                , description: "Alla artiklar på hbl.fi"
                }
              , { title: "Nyhetsappen HBL Nyheter"
                , description: "Nyheter på mobilen och surfplattan, pushnotiser"
                }
              , { title: "Digitalt månadsbrev"
                , description: "Nyheter & förmåner"
                }
              ]
          }

    -- TODO: maybe we don't want to deal at all with the error cases
    -- and we want to throw them away?
    renderElement :: Either String BodyElement -> JSX
    renderElement = case _ of
      Left _   -> mempty
      Right el -> case el of
        Html content ->
          -- Can't place div's under p's, so if div, create div.
          -- This is usually case with embeds
          if isDiv content
          then
            DOM.div
              { dangerouslySetInnerHTML: { __html: content }
              , className: block <> " " <> block <> "__html"
              }
          else
            DOM.p
              { dangerouslySetInnerHTML: { __html: content }
              , className: block <> " " <> block <> "__html"
              }
        Headline str -> DOM.h4
          { className: block <> " " <> block <> "__subheadline"
          , children: [ DOM.text str ]
          }
        Image img -> DOM.div
          { className: block
          , children: [ renderImage img ]
          }
        Box boxData ->
          DOM.div
            { className: block <> " " <> block <> "__factbox"
            , children:
                [ box
                    { headline: boxData.headline
                    , title: boxData.title
                    , content: boxData.content
                    , brand: props.brand
                    }
                ]
            }
        Footnote footnote -> DOM.p
            { className: block <> " " <> block <> "__footnote"
            , children: [ DOM.text footnote ]
            }
        Quote quote -> DOM.q
            { className: block <> " " <> block <> "__quote"
            , children: [ DOM.text quote ]
            }
        Question question -> DOM.p
            { className: block <> " " <> block <> "__question"
            , children: [ DOM.text question ]
            }
      where
        block = "article-element"
        isDiv = (String.contains (Pattern "<div"))
