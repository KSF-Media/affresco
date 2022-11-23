module Prenumerera.Page.GiftRedeem where

import Prelude

import Bottega.GiftApi as Api
import Bottega.Models as Bottega.Models
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff as Aff
import KSF.InputField as InputField
import KSF.Spinner as Spinner
import Prenumerera.Gift (Gift)
import Prenumerera.Package (Package)
import Prenumerera.Package.Description (Description)
import Prenumerera.Package.Description as Description
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, useEffect, useState', (/\))
import React.Basic.Hooks as React
import Data.Map as Map

type Props =
  { giftCode :: Maybe String
  , startPurchase :: Gift -> Package -> Description -> Effect Unit
  , cancel :: Effect Unit
  , packages :: Array Package
  }

component :: Component Props
component = do
  React.component "GiftRedeem" $ \props@{ giftCode: initialGiftCode, packages, startPurchase, cancel } -> React.do
    loading /\ setLoading <- useState' $ isJust initialGiftCode
    gift /\ setGift <- useState' Nothing
    giftCode /\ setGiftCode <- useState' initialGiftCode
    giftDialog /\ setGiftDialog <- useState' ""
    giftError /\ setGiftError <- useState' false
    let giftDetails = do
          code <- giftCode
          (g :: Bottega.Models.Gift) <- gift
          package <- Array.find (\p -> p.id == g.package) packages
          description <- Description.lookup package.id
          pure { gift: { code, owner: g.owner }, package, description }
    useEffect giftCode do
      case giftCode of
        Just code -> do
          setLoading true
          setGiftError false
          Aff.launchAff_ do
            maybeGift <- hush <$> Api.getGift code
            liftEffect do
              setLoading false
              setGift maybeGift
              -- TODO more errors
              when (isNothing maybeGift) $ setGiftError true
        -- Either initial state when no gift code given or after user
        -- reset
        Nothing -> pure unit
      pure $ pure unit
    pure $ render $ case { loading, gift, giftCode, giftError, giftDetails } of
      { giftError: true } -> renderError $ setGiftError false *> setGiftCode Nothing
      { loading: true } -> Spinner.loadingSpinner
      { gift: Nothing } -> renderEnterCode giftDialog setGiftDialog (setGiftCode $ sanitizeGift giftDialog)
      { giftDetails: Just g } -> renderProceed g.description cancel $ startPurchase g.gift g.package g.description
      _ -> renderError $ setGiftCode Nothing
  where
    -- Just in case
    sanitizeGift g =
      case replaceAll (Pattern "[^a-zA-Z0-9]") (Replacement "") g of
        "" -> Nothing
        x -> Just x

render :: JSX -> JSX
render innerContent =
  DOM.div
    { className: "container-fluid ksf-block-giftredeem"
    , children:
        [ DOM.div
            { className: "container ksf-block"
            , children: [ innerContent ]
            }
        ]
    }

renderEnterCode :: String -> (String -> Effect Unit) -> Effect Unit -> JSX
renderEnterCode code setCode submit =
  fold
  [ DOM.h2
      { className: "hero-text"
      , children: [ DOM.span_ [ DOM.text "Kvalitetsjournalistik" ]
                  , DOM.text " när, var och hur du vill"
                  ]
      }
  , DOM.h3_ [ DOM.text "Lös in din värdekod" ]
  , DOM.form
      { children:
          [ DOM.div
              { className: "row"
              , children:
                  [ InputField.inputField
                      { type_: InputField.Text
                      , label: Just "Värdekod"
                      , placeholder: "Värdekod"
                      , name: "giftCode"
                      , value: Just code
                      , pattern: "^[a-zA-Z0-9]+$"
                      , onChange: setCode <<< fromMaybe ""
                      , validationError: Nothing
                      }
                  ]
              }
          , DOM.div
              { className: "row"
              , children:
                  [ DOM.input
                      { type: "submit"
                      , className: "submit-button"
                      , value: "Fortsätt"
                      , disabled: code /= ""
                      }
                  ]
              }
          ]
      , onSubmit: capture_ submit
      }
  ]

renderProceed :: Description -> Effect Unit -> Effect Unit -> JSX
renderProceed description cancel next =
  fold
  [ DOM.h2
      { className: "hero-text"
      , children: [ DOM.span_ [ DOM.text "Kvalitetsjournalistik" ]
                  , DOM.text " när, var och hur du vill"
                  ]
      }
  , DOM.h3_ [ DOM.text "Lunasta seuraava lahjatilaus" ]
  , DOM.div
      { className: "row"
      , children:
          [ DOM.h3_ [ DOM.text $ description.brandLong <> description.descriptionShort ]
          ]
      }
  , description.descriptionLong
  , DOM.button
      { className: "submit-button"
      , children: [ DOM.text "Fortsätt" ]
      , onClick: capture_ next
      }
  , DOM.div
      { className: "row"
      , children:
          [ DOM.text "eller "
          , DOM.a
              { children: [ DOM.text "avbryt" ]
              , href: "/"
              , onClick: capture_ cancel
              }
          ]
      }
  ]

renderError :: Effect Unit -> JSX
renderError reset =
  fold
  [ DOM.h2_ [ DOM.text "Något gick fel!" ]
  , DOM.a
      { children: [ DOM.text "Försok igen" ]
      , href: "/"
      , onClick: capture_ reset
      }
  ]

{-
    useEffectOnce do
      case initialGiftCode of
        Nothing -> pure unit
        Just code -> do
          setLoading true
            case maybeGift of
              Just g -> do
                liftEffect $ setGift g
      pure $ pure unit
    useEffect giftCode $ do
      case giftCode of
        Just code -> do
          
    useEffect gift $ do
      pure $ pure unit
    
    pure $ render (if gift.own then Left giftCode else Right startPurchase) gift descripiton
-}

{-
render :: Either String (Effect Unit) -> Gift -> Description -> JSX
render start gift description =
  DOM.div
    { className: "container-fluid ksf-block-giftredeem"
    , children:
        [ DOM.div
            { className: "container ksf-block"
            , children:
                [ DOM.h2
                    { className: "hero-text"
                    , children: [ DOM.span_ [ DOM.text "Kvalitetsjournalistik" ]
                                , DOM.text " när, var och hur du vill"
                                ]
                    }
                , DOM.h3_ [ DOM.text $ if isJust startIfNotOwn then "Gift Redeem" else "Gift Details" ]
                , DOM.div
                    { className "ksf-block-giftdescription"
                    , children: 
-}
