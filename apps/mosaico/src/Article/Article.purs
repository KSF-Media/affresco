module Mosaico.Article where

import Prelude

import Bottega.Models.Order (OrderSource(..))
import Control.Alt ((<|>))
import Data.Array (cons, head, insertAt, length, snoc, partition, (!!))
import Data.Either (Either(..), fromRight, isRight)
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep.RecordToSum as Record
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (un, unwrap)
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
import Lettera.Models (Article, ArticleStub, BodyElement(..), FullArticle(..), Image, LocalDateTime(..), Tag(..), fromFullArticle, isErrorArticle, tagToURIComponent)
import Mosaico.Ad as Ad
import Mosaico.Article.Box (box)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
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
  , onTagClick :: Tag -> EventHandler
  , user :: Maybe User
  , uuid :: Maybe String
  }

type State =
  { body :: Array (Either String BodyElement)
  , article :: Maybe FullArticle
  , title :: String
  , mainImage :: Maybe Image
  , tags :: Array Tag
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
        bodyWithoutErrors = removeBodyErrors $ state.body
        bodyWithoutAd = map renderElement bodyWithoutErrors
        bodyWithAd = map renderElement
          <<< insertAdsIntoBodyText articleMiddle1Desktop articleMiddle2Desktop $ bodyWithoutErrors
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
            -- We don't want to be able to share error articles
            , guard (maybe false (not <<< isErrorArticle) state.article)
                DOM.section
                  { className: "mosaico-article__tag-n-share"
                  , children:
                      [ DOM.div
                          { className: "mosaico-article__tag-n-premium"
                          , children:
                              [ foldMap renderTag $ head tags
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
                            bodyWithoutAd
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

    renderTag tag =
      DOM.a
        { className: "mosaico-article__tag color-" <> props.brand
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

    articleMiddle1Desktop = "article_middle_1_desktop"
    articleMiddle2Desktop = "article_middle_2_desktop"

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
        , askAccountAlways: false
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

    renderElement :: BodyElement -> JSX
    renderElement el = 
      case el of
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
        Ad contentUnit ->
          Ad.ad {
            contentUnit
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
        isDiv = isElem "<div"
        isBlockquote = isElem "<blockquote"
        -- Does the string start with wanted element
        isElem elemName elemString =
          Just 0 == String.indexOf (Pattern elemName) elemString
        

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
          | otherwise    = case body' !! (i-1) of
            Just (Html _) -> case body' !! i of
              Just (Html _) -> i
              _             -> findAdSpace body' (i+1)
            _             -> findAdSpace body' (i+1)

    removeBodyErrors :: Array (Either String BodyElement) -> Array BodyElement
    removeBodyErrors body = 
      let partitioned = partition isRight body
      in removeEither <$> partitioned.yes
      where
        removeEither = fromRight (Html "")
