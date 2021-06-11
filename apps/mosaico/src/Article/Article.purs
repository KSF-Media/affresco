module Mosaico.Article where

import Prelude

import Control.Alt ((<|>))
import Data.Array (cons, head, snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep.RecordToSum as Record
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api.Package (CampaignLengthUnit(..))
import KSF.Helpers (formatArticleTime)
import KSF.Paper (Paper(..))
import KSF.User (User)
import KSF.Vetrina as Vetrina
import Lettera.Models (ArticleStub, BodyElement(..), FullArticle(..), Image, LocalDateTime(..), fromFullArticle)
import Mosaico.Article.Box (box)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React


foreign import someIcons ::
  { facebook :: String
  , twitter :: String
  , linkedin :: String
  , mail :: String
  , whatsapp :: String
  }

type Self =
  { state :: State
  , setState :: (State -> State) -> Effect Unit
  , props :: Props
  }

type Props =
  { brand :: String
  , affArticle :: Aff FullArticle
  , articleStub :: Maybe ArticleStub
  , onLogin :: Effect Unit
  , user :: Maybe User
  }

type State =
  { body :: Array (Either String BodyElement)
  , article :: Maybe FullArticle
  , title :: String
  , mainImage :: Maybe Image
  , tags :: Array String
  , preamble :: Maybe String
  }

articleComponent :: Component Props
articleComponent = do
  component "Article" \props -> React.do
    let initialState =
          { body: []
          , article: Nothing
          , title: maybe mempty (\articleStub -> articleStub.title) props.articleStub
          , mainImage: do
              articleStub <- props.articleStub
              articleStub.listImage
          , tags: maybe mempty (\articleStub -> articleStub.tags) props.articleStub
          -- , preamble: maybe Nothing (\articleStub -> articleStub.preamble) props.articleStub
          , preamble: (\articleStub -> articleStub.preamble) =<< props.articleStub
          }
    state /\ setState <- useState initialState 

    useEffectOnce do
      loadArticle setState props.affArticle
      pure mempty

    -- If user logs in / logs out, reload the article.
    -- NOTE: We simply compare the email attribute of `User`
    -- as not every attribute of `User` implements `Eq`
    useEffect (_.email <$> props.user) do
      loadArticle setState props.affArticle
      pure mempty

    pure $ render { state, setState, props }

loadArticle :: ((State -> State) -> Effect Unit) -> Aff FullArticle -> Effect Unit
loadArticle setState affArticle =
  Aff.launchAff_ do
    a <- affArticle
    let realArticle = fromFullArticle a
    liftEffect $ setState \s -> s
      { article = Just a
      , body = map Record.toSum $ _.body $ fromFullArticle a
      , mainImage = realArticle.mainImage
      , title = realArticle.title
      , tags = realArticle.tags
      , preamble = realArticle.preamble
      }

renderImage :: Image -> JSX
renderImage img =
  DOM.div
    { className: "mosaico--article--image"
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
        title = fromMaybe mempty $ map _.title props.articleStub <|> map _.title letteraArticle
        tags = fromMaybe mempty $ map _.tags props.articleStub <|> map _.tags letteraArticle
        mainImage = (_.listImage =<< props.articleStub) <|> (_.mainImage =<< letteraArticle)
    in DOM.div
      { className: "mosaico--article"
      , children:
        [ DOM.div
            { className: "mosaico--tag color-" <> props.brand
            , children: [ DOM.text $ fromMaybe "" (head tags) ]
            }
        , DOM.h1
            { className: "mosaico--article--title title"
            , children: [ DOM.text title ]
            }
        , foldMap renderImage mainImage
        , DOM.div
          { className: "mosaico--article--preamble"
          , children: [ DOM.p_ [ DOM.text $ fromMaybe mempty state.preamble ] ]
          }
        , DOM.div
            { className: "mosaico--article-times-and-author"
            , children:
                [ foldMap renderAuthors $ _.authors <$> letteraArticle
                , foldMap articleTimestamps letteraArticle
                ]
            }
        , DOM.ul
            { className: "mosaico-article__some"
            , children:
              [ DOM.li_
                [ DOM.a
                  { href: "#"
                  , children: [ DOM.img { src: someIcons.facebook } ]
                  }
                ]
              , DOM.li_
                [ DOM.a
                  { href: "#"
                  , children: [ DOM.img { src: someIcons.twitter } ]
                  }
                ]
              , DOM.li_
                [ DOM.a
                  { href: "#"
                  , children: [ DOM.img { src: someIcons.linkedin } ]
                  }
                ]
              , DOM.li_
                [ DOM.a
                  { href: "#"
                  , children: [ DOM.img { src: someIcons.whatsapp } ]
                  }
                ]
              , DOM.li_
                [ DOM.a
                  { href: "#"
                  , children: [ DOM.img { src: someIcons.mail } ]
                  }
                ]
              ]
            }
        , DOM.div
            { className: "mosaico--article--body "
            , children: case state.article of
              (Just (PreviewArticle previewArticle)) ->
                paywallFade
                `cons` map renderElement state.body
                `snoc` vetrina
              (Just (FullArticle fullArticle)) ->
                map renderElement state.body
              _ -> mempty
          }
      ]
    }
  where
    renderAuthors authors =
      DOM.div
        { className: "mosaico--article-authors"
        , children:
            map (DOM.span_ <<< Array.singleton <<< DOM.text <<< _.byline) authors
            `snoc` premiumBadge
        }
      where
        premiumBadge =
          guard (maybe false (_.premium <<< fromFullArticle) state.article)
          DOM.div
            { className: "mosaico--article--premium background-hbl"
            , children: [ DOM.text "premium" ]
            }

    articleTimestamps { publishingTime, updateTime } =
      DOM.div
        { className: "mosaico--article-timestamps"
        , children:
            [ foldMap renderPublishingTime publishingTime
            , foldMap renderUpdateTime updateTime
            ]
        }
      where
        renderPublishingTime (LocalDateTime time) =
          DOM.div
            { className: "mosaico--article-published-timestamp"
            , children: [ DOM.text $ "Pub. " <> formatArticleTime time ]
            }
        renderUpdateTime (LocalDateTime time) =
          DOM.div
            { className: "mosaico--article-updated-timestamp"
            , children: [ DOM.text $ "Uppd. " <> formatArticleTime time ]
            }

    paywallFade =
        DOM.div { className: "mosaico--article-fading-body" }

    vetrina =
      Vetrina.vetrina
        { onClose: Just $ loadArticle setState props.affArticle
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
        , loadingContainer: Nothing
        , accessEntitlements: Set.fromFoldable ["hbl-365", "hbl-web"]
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
      Left err -> mempty
      Right el -> case el of
        Html content -> DOM.p
          { dangerouslySetInnerHTML: { __html: content }
          , className: "html"
          }
        Headline str -> DOM.h4
          { className: "headline"
          , children: [ DOM.text str ]
          }
        Image img -> renderImage img
        Box boxData ->
          DOM.div
            { className: "factbox"
            , children:
                [ box
                    { headline: boxData.headline
                    , title: boxData.title
                    , content: boxData.content
                    , brand: props.brand
                    }
                ]
            }
        other -> DOM.p_ [ DOM.text $ show other ]
