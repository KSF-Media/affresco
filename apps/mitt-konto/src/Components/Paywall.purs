module MittKonto.Components.Paywall where

import Prelude

import Data.Int (fromString)
import Data.JSDate (JSDate, now)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
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

paywall :: PushStateInterface -> Sentry.Logger -> Component Props
paywall router logger = do
  currentDate <- now
  component "Paywall" \ {} -> React.do
    days     /\ setDays     <- useState' 0
    hours    /\ setHours    <- useState' 0
    minutes  /\ setMinutes  <- useState' 0
    products /\ setProducts <- useState
      { hblEpaper: true
      , onEpaper: true
      , vnEpaper: true
      , juniorEpaper: true
      }
    let
      submitHandler :: EventHandler
      submitHandler = handler_ $ User.openPaywall days hours minutes
    pure $ DOM.div
      { children:
        [ DOM.h1_ [ DOM.text "Öppna/stäng betalmur för" ]
        , renderDays    days    setDays
        , renderHours   hours   setHours
        , renderMinutes minutes setMinutes
--        , renderProducts products products
        , DOM.button
            { children: [ DOM.text "Skicka" ]
            , onClick: submitHandler
            }
        ]
      }

renderDays :: Int -> (Int -> Effect Unit) -> JSX
renderDays days setDays =
  DOM.label
    { children:
        [ DOM.input
            { type: "text"
            , value: show days
            , onChange: setNumber 365 setDays
            }
        , DOM.text "&nbsp;dagar"
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
        , DOM.text "&nbsp;timmar"
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
        , DOM.text "&nbsp;minuter"
        ]
      , style: DOM.css { marginRight: "1rem" }
    }

