module MittKonto.Components.Paywall where

import Prelude

import Data.Array ((:), filter, findIndex, length, modifyAt, null)
import Data.Foldable (intercalate)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
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
  [ { name: "hbl-365", label: "HBL-artiklar i appen HBL 365", selected: false }
  , { name: "hbl-epaper", label: "HBL e-tidning (e-tidningen i appen och på webben)", selected: false }
  , { name: "hbl-web", label: "HBL Premiumartiklar på hbl.fi", selected: false }
  , { name: "junior-epaper", label: "HBL Junior (e-tidningen i appen och på webben)", selected: false }
  , { name: "on-365", label: "Östnyland-artiklar i appen HBL 365", selected: false }
  , { name: "on-epaper", label: "Östnyland e-tidning (e-tidningen i appen och på webben)", selected: false }
  , { name: "on-web", label: "Östnyland Premiumartiklar på ostnyland.fi", selected: false }
  , { name: "vn-365", label: "Västra Nyland-artiklar i appen HBL 365", selected: false }
  , { name: "vn-epaper", label: "Västra Nyland e-tidning (e-tidningen i appen och på webben)", selected: false }
  , { name: "vn-web", label: "Västra Nyland Premiumartiklar på vastranyland.fi", selected: false }
  ]

paywall :: PushStateInterface -> Sentry.Logger -> Component Props
paywall _router _logger = do
  component "Paywall" \ {} -> React.do
    days     /\ setDays     <- useState' 0
    hours    /\ setHours    <- useState' 0
    minutes  /\ setMinutes  <- useState' 0
    products /\ setProducts <- useState initialProducts
    openings /\ setOpenings <- useState' Nothing
    era      /\ setEra      <- useState 0
    useEffect era $ do
      fiber <- Aff.launchAff $
        liftEffect <<< setOpenings <<< Just =<< User.getPaywallOpenings
      pure $ Aff.launchAff_ $ Aff.killFiber (error "component closed") fiber
    let
      selection = map _.name $ filter _.selected products
      deletionHandler :: Int -> EventHandler
      deletionHandler id = handler_ $ Aff.launchAff_ $ do
        User.deletePaywallOpening id
        liftEffect $ setEra (_ + 1)
      selectAllHandler :: EventHandler
      selectAllHandler = capture_ $ setProducts $
        map _{ selected = true }
      submitHandler :: EventHandler
      submitHandler = capture_ $ Aff.launchAff_ $ do
        User.openPaywall days hours minutes selection
        liftEffect $ setEra (_ + 1)
    pure $ DOM.form
      { children:
        [ DOM.h1_ [ DOM.text "Inställningar för betalvägg" ]
        , DOM.hr {}
        , DOM.h2_ [ DOM.text "Öppna betalvägg" ]
        , renderDays     days     setDays
        , renderHours    hours    setHours
        , renderMinutes  minutes  setMinutes
        , renderProducts products setProducts
        , DOM.button
            { children: [ DOM.text "Alla" ]
            , onClick: selectAllHandler
            , disabled: length selection == length products
            }
        , DOM.button
            { children: [ DOM.text "Skicka" ]
            , type: "submit"
            , disabled: days + hours + minutes == 0 || null selection
            }
        , DOM.hr {}
        , DOM.h2_ [ DOM.text "Just nu öppna betalväggar" ]
        , maybe loadingSpinner (renderCurrentOpenings deletionHandler) openings
        ]
      , onSubmit: submitHandler
      }

setNumber :: Int -> (Int -> Effect Unit) -> EventHandler
setNumber max setter = handler targetValue \value ->
  maybe (pure unit) setter (value >>= fromString >>= handleBounds)
  where
    handleBounds n
      | n < 0     = Just 0
      | n > max   = Just max
      | otherwise = Just n

renderDays :: Int -> (Int -> Effect Unit) -> JSX
renderDays days setDays =
  DOM.label
    { children:
        [ DOM.input
            { type: "number"
            , value: show days
            , onChange: setNumber 365 setDays
            , style: DOM.css { width: "3rem" }
            }
        , DOM.text " dagar"
        ]
      , style: DOM.css { marginRight: "1rem" }
    }

renderHours :: Int -> (Int -> Effect Unit) -> JSX
renderHours hours setHours =
  DOM.label
    { children:
        [ DOM.input
            { type: "number"
            , value: show hours
            , onChange: setNumber 23 setHours
            , style: DOM.css { width: "3rem" }
            }
        , DOM.text " timmar"
        ]
      , style: DOM.css { marginRight: "1rem" }
    }

renderMinutes :: Int -> (Int -> Effect Unit) -> JSX
renderMinutes minutes setMinutes =
  DOM.label
    { children:
        [ DOM.input
            { type: "number"
            , value: show minutes
            , onChange: setNumber 59 setMinutes
            , style: DOM.css { width: "3rem" }
            }
        , DOM.text " minuter"
        ]
      , style: DOM.css { marginRight: "1rem" }
    }

toggleProduct :: String -> Products -> Products
toggleProduct name products =
  fromMaybe initialProducts $ do
    index <- findIndex (\p -> p.name == name) products
    let modify p = p { selected = not p.selected }
    modifyAt index modify products

renderProducts
  :: Products
  -> ((Products -> Products) -> Effect Unit)
  -> JSX
renderProducts products setProducts =
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
                  , onChange: handler_ $ setProducts (toggleProduct name)
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