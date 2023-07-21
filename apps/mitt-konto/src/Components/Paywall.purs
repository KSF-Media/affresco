module MittKonto.Components.Paywall where

import Prelude

import Data.Array ((:), filter, findIndex, length, modifyAt)
import Data.DateTime (DateTime(..), date, time)
import Data.Foldable (intercalate)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (nowDateTime)
import KSF.Api.Entitlements (PaywallOpening)
import KSF.Sentry as Sentry
import KSF.Spinner (loadingSpinner)
import KSF.User as User
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, targetValue)
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Component, component, useEffect, useState, useState', (/\))
import React.Basic.Hooks as React
import Routing.PushState (PushStateInterface)

type Props = {}
type Product = { name :: String, label :: String, selected :: Boolean }
type Products = Array Product

initialProducts :: Products
initialProducts =
  [ { name: "hbl-epaper", label: "HBL e-tidningen på webben", selected: false }
  , { name: "hbl-web", label: "HBL premiumartiklar", selected: false }
  , { name: "on-epaper", label: "Östnyland e-tidningen på webben", selected: false }
  , { name: "on-web", label: "Östnyland premiumartiklar", selected: false }
  , { name: "vn-epaper", label: "Västra Nyland e-tidningen på webben", selected: false }
  , { name: "vn-web", label: "Västra Nyland premiumartiklar", selected: false }
  , { name: "junior-epaper", label: "HBL Junior e-tidningen på webben", selected: false }
  ]

paywall :: PushStateInterface -> Sentry.Logger -> Component Props
paywall _router _logger = do
  rightNow <- nowDateTime
  let initialDate = date rightNow
      initialTime = time rightNow
      formatter = fromFoldable
                  [ YearFull
                  , Placeholder "-"
                  , MonthTwoDigits
                  , Placeholder "-"
                  , DayOfMonthTwoDigits
                  , Placeholder "T"
                  , Hours24
                  , Placeholder ":"
                  , MinutesTwoDigits
                  ]
      initialDateTimeString = format formatter (DateTime initialDate initialTime)
  component "Paywall" \ {} -> React.do
    startAt  /\ setStartAt  <- useState' initialDateTimeString
    endAt    /\ setEndAt    <- useState' initialDateTimeString
    products /\ setProducts <- useState initialProducts
    openings /\ setOpenings <- useState' Nothing
    era      /\ setEra      <- useState 0
    useEffect era $ do
      fiber <- Aff.launchAff $
        liftEffect <<< setOpenings <<< Just =<< User.getPaywallOpenings
      pure $ Aff.launchAff_ $ Aff.killFiber (error "component closed") fiber
    let
      selectedProducts = map _.name $ filter _.selected products
      deletionHandler :: Int -> EventHandler
      deletionHandler id = handler_ $ Aff.launchAff_ $ do
        User.deletePaywallOpening id
        liftEffect $ setEra (_ + 1)
      selectAllHandler :: EventHandler
      selectAllHandler = capture_ $ setProducts $
        map _{ selected = true }
      submitHandler :: EventHandler
      submitHandler = capture_ $ Aff.launchAff_ $ do
        User.openPaywall
          { startAt: startAt <> ":00.000Z"
          , endAt: endAt <> ":00.000Z"
          , onlyToProducts: selectedProducts
          }
        liftEffect $ setEra (_ + 1)
    pure $ DOM.form
      { children:
        [ DOM.h1_ [ DOM.text "Inställningar för betalvägg" ]
        , DOM.hr {}
        , DOM.h2_ [ DOM.text "Öppna betalvägg" ]
        , renderStartAt initialDateTimeString startAt setStartAt
        , renderEndAt startAt endAt setEndAt
        , renderProducts products setProducts
        , DOM.button
            { children: [ DOM.text "Alla" ]
            , onClick: selectAllHandler
            , disabled: length selectedProducts == length products
            }
        , DOM.button
            { children: [ DOM.text "Skicka" ]
            , type: "submit"
            }
        , DOM.hr {}
        , DOM.h2_ [ DOM.text "Just nu öppna betalväggar" ]
        , maybe loadingSpinner (renderCurrentOpenings deletionHandler) openings
        ]
      , onSubmit: submitHandler
      }

renderStartAt :: String -> String -> (String -> Effect Unit) -> JSX
renderStartAt min startAt setStartAt =
  DOM.span_
    [ DOM.text "från "
    , DOM.input
      { type: "datetime-local"
      , value: startAt
      , onChange: handler targetValue $ maybe (pure unit) setStartAt
      , min
      }
    , DOM.text " (UTC)"
    ]

renderEndAt :: String -> String -> (String -> Effect Unit) -> JSX
renderEndAt min endAt setEndAt =
  DOM.span_
    [ DOM.text " till "
    , DOM.input
      { type: "datetime-local"
      , value: endAt
      , onChange: handler targetValue $ maybe (pure unit) setEndAt
      , min
      }
    , DOM.text " (UTC)"
    ]

toggleProduct :: String -> Products -> Products
toggleProduct name products =
  fromMaybe initialProducts $ do
    index <- findIndex (\p -> p.name == name) products
    let modify p = p { selected = not p.selected }
    modifyAt index modify products

renderProducts :: Products -> ((Products -> Products) -> Effect Unit) -> JSX
renderProducts products modifyProducts =
  DOM.dl
    { children:
        DOM.dt_ [ DOM.text "Markera var du vill öppna betalväggen"] : map renderProduct products
    , style: DOM.css { listStyleType: "none" }
    }
  where
    renderProduct { name, label, selected } =
      DOM.dl_
      [ DOM.label
          { children:
              [ DOM.input
                  { type: "checkbox"
                  , value: name
                  , checked: selected
                  , onChange: handler_ $ modifyProducts (toggleProduct name)
                  }
              , DOM.text label
              ]
          }
      ]

renderCurrentOpenings
  :: (Int -> EventHandler)
  -> Array PaywallOpening
  -> JSX
renderCurrentOpenings mkHandler openings =
  let
    renderOpening opening = DOM.tr
      { children:
          [ DOM.td_ [ DOM.text (show opening.id) ]
          , DOM.td_ [ DOM.text opening.startAt ]
          , DOM.td_ [ DOM.text opening.endAt ]
          , DOM.td_
              [ DOM.text
                  (case toMaybe opening.onlyToProducts of
                      Just ps | length ps < length initialProducts ->
                        intercalate ", " ps
                      _ ->
                        "Alla") ]
          , DOM.td_
              [ DOM.button
                  { children: [DOM.text "Radera"]
                  , onClick: mkHandler opening.id
                  }
              ]
          ]
      }
  in
    DOM.table
      { children:
        [ DOM.thead_
          [ DOM.tr_
            [ DOM.th { children: [ DOM.text "ID" ] }
            , DOM.th { children: [ DOM.text "Från" ] }
            , DOM.th { children: [ DOM.text "Till" ] }
            , DOM.th { children: [ DOM.text "Plats" ] }
            , DOM.th { children: [] }
            ]
          ]
        , DOM.tbody_ $ map renderOpening openings
        ]
      , style: DOM.css { borderSpacing: "1rem" }
      }
