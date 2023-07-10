module MittKonto.Components.Paywall where

import Prelude

import Data.Array (filter, findIndex, modifyAt, null)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import KSF.Sentry as Sentry
import KSF.User as User
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Component, component, useState, useState', (/\))
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
paywall _router logger = do
  component "Paywall" \ {} -> React.do
    days     /\ setDays     <- useState' 0
    hours    /\ setHours    <- useState' 0
    minutes  /\ setMinutes  <- useState' 0
    products /\ setProducts <- useState initialProducts
    let
      submitHandler :: EventHandler
      submitHandler = handler_
        $ when (not $ null selection)
        $ User.openPaywall days hours minutes (map (\p -> p.name) $ selection)
        where
          selection = filter (\p -> p.selected) products
    pure $ DOM.div
      { children:
        [ DOM.h1_ [ DOM.text "Öppna betalmur" ]
        , renderDays     days     setDays
        , renderHours    hours    setHours
        , renderMinutes  minutes  setMinutes
        , renderProducts products setProducts
        , DOM.button
            { children: [ DOM.text "Skicka" ]
            , onClick: submitHandler
            }
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
  DOM.label
    { children:
      [ DOM.ul { children: map renderProduct products } ]
    }
  where
    renderProduct { name, label, selected } =
      DOM.li
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
