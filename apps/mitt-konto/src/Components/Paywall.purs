module MittKonto.Components.Paywall where

import Prelude

import Data.Array (filter, findIndex, modifyAt, null)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Uncurried (mkEffectFn1)
import Data.JSDate (JSDate, now, toUTCString)
import KSF.Api.Entitlements (PaywallOpening)
import KSF.Sentry as Sentry
import KSF.Spinner (loadingSpinner)
import KSF.User as User
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Component, component, useEffect, useState, useState', (/\))
import React.Basic.Hooks as React
import Routing.PushState (PushStateInterface)

type Props = {}
type Product = { name :: String, label :: String, selected :: Boolean }
type Products = Array Product

initialProducts :: Products
initialProducts =
  [ { name: "hbl-365", label: "HBL 365", selected: false }
  , { name: "hbl-epaper", label: "HBL e-tidning", selected: false }
  , { name: "hbl-web", label: "HBL webbplats", selected: false }
  , { name: "junior-epaper", label: "HBL Junior e-tidning", selected: false }
  , { name: "on-365", label: "Östra Nyland 365", selected: false }
  , { name: "on-epaper", label: "Östra Nyland e-tidning", selected: false }
  , { name: "on-web", label: "Östra Nyland webbplats", selected: false }
  , { name: "vn-365", label: "Västra Nyland 365", selected: false }
  , { name: "vn-epaper", label: "Västra Nyland e-tidning", selected: false }
  , { name: "vn-web", label: "Västra Nyland webbplats", selected: false }
  ]

paywall :: PushStateInterface -> Sentry.Logger -> Component Props
paywall _router _logger = do
  currentDate <- now
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
      selection = map (\p -> p.name) $ filter (\p -> p.selected) products
      submitHandler :: EventHandler
      submitHandler = handler_ $ Aff.launchAff_ $ do
        User.openPaywall days hours minutes selection
        liftEffect $ setEra (_ + 1)
    pure $ DOM.div
      { children:
        [ DOM.h1_ [ DOM.text "Betalmur inställningar" ]
        , DOM.hr {}
        , DOM.h2_ [ DOM.text "Öppna betalmur" ]
        , renderDays     days     setDays
        , renderHours    hours    setHours
        , renderMinutes  minutes  setMinutes
        , renderProducts products setProducts
        , DOM.button
            { children: [ DOM.text "Skicka" ]
            , onClick: submitHandler
            , disabled: days + hours + minutes == 0 || null selection
            }
        , DOM.hr {}
        , DOM.h2_ [ DOM.text "Nuvarande betalmur öppningar" ]
        , maybe loadingSpinner renderCurrentOpenings openings
        ]
      }

setNumber :: Int -> (Int -> Effect Unit) -> EventHandler
setNumber max setter = handler targetValue \value ->
  maybe (pure unit) setter (value >>= handleEmpty >>= fromString >>= handleBounds)
  where
    handleEmpty s = Just case s of
      "" -> "0"
      _  -> s
    handleBounds n
      | n < 1     = Just 0
      | n > max   = Just max
      | otherwise = Just n

renderDays :: Int -> (Int -> Effect Unit) -> JSX
renderDays days setDays =
  DOM.label
    { children:
        [ DOM.input
            { type: "text"
            , value: show days
            , onChange: setNumber 365 setDays
            , style: DOM.css { width: "2rem" }
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
            { type: "text"
            , value: show hours
            , onChange: setNumber 23 setHours
            , style: DOM.css { width: "2rem" }
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
            { type: "text"
            , value: show minutes
            , onChange: setNumber 59 setMinutes
            , style: DOM.css { width: "2rem" }
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
  DOM.ul { children: map renderProduct products }
  where
    renderProduct { name, label, selected } =
      DOM.li_
      [ DOM.label
          { children:
              [ DOM.input
                  { type: "checkbox"
                  , value: name
                  , checked: selected
                  , onChange: mkEffectFn1 (\_ -> setProducts (toggleProduct name))
                  }
              , DOM.text label
              ]
          }
      ]

renderCurrentOpenings :: Array PaywallOpening -> JSX
renderCurrentOpenings openings =
  let
    renderOpening opening = DOM.tr
      { children:
          [ DOM.td_ [ DOM.text (show opening.id) ]
          , DOM.td_ [ DOM.text (toUTCString opening.startAt) ]
          , DOM.td_ [ DOM.text (toUTCString opening.endAt) ]
          , DOM.td_ [ DOM.text (show opening.onlyToProducts) ]
          , DOM.td_ [ DOM.text "radera" ]
          ]
      }
  in
    DOM.table
      { children:
          [ DOM.thead
              { children:
                  [ DOM.tr
                      { children:
                          [ DOM.th_ [ DOM.text "ID" ]
                          , DOM.th_ [ DOM.text "Från" ]
                          , DOM.th_ [ DOM.text "Till" ]
                          , DOM.th_ [ DOM.text "Produkter" ]
                          , DOM.th_ []
                          ]
                      }
                  ]
              }
          , DOM.tbody
              { children: map renderOpening openings
              }
          ]
      }
