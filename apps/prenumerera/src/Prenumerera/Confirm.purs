module Prenumerera.Confirm where

import Prelude

import Control.Comonad ((<<=))
import Data.Array (catMaybes)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Persona as Persona
import React.Basic (JSX, StateUpdate(..), element, make, send)
import React.Basic as React
import React.Basic.DOM as DOM
import Router (Location)
import Router as Router
import Prenumerera.Prenumerera (Product)
import Unsafe.Coerce (unsafeCoerce)

type Self = React.Self Props State Action

type Props = { location ::  Location (Maybe LocationState) }

type JSProps = { location :: Location (Nullable LocationJsState) }

type State = LocationState

data Action =
  SetProduct Product
  | SetUser Persona.User
  | SetPayment Payment

type Payment =
  { startDate :: Maybe String
  , paymentMethod :: Maybe String
  , price :: Maybe Number
  , surcharge :: Maybe Number
  }

type PaymentJS =
  { startDate :: Nullable String
  , paymentMethod :: Nullable String
  , price :: Nullable Number
  , surcharge :: Nullable Number
  }

type LocationState   = { product :: Maybe Product, user :: Maybe Persona.User, payment :: Maybe Payment }
type LocationJsState = { product :: Nullable Product, user :: Nullable Persona.User, payment :: Nullable PaymentJS }

fromJSProps :: JSProps -> Props
fromJSProps { location: { key, pathname, search, hash, state } } =
  { location:
        { key
        , pathname
        , search
        , hash
        , state: setState <$> toMaybe state
        }
  }
  where
    setState :: LocationJsState -> LocationState
    setState state_ =
      { product: toMaybe state_.product
      , user: toMaybe state_.user
      , payment: convertPayment <$> toMaybe state_.payment
      }
    convertPayment payment =
      { startDate: toMaybe payment.startDate
      , paymentMethod: toMaybe payment.paymentMethod
      , price: toMaybe payment.price
      , surcharge: toMaybe payment.surcharge
      }

component :: React.Component Props
component = React.createComponent "Confirm"

jsComponent :: React.ReactComponent JSProps
jsComponent =
  React.toReactComponent fromJSProps component { initialState, render, update, didMount }

confirm :: Props -> JSX
confirm = make component
  { initialState
  , render
  , update
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self@{ props: { location: { state: Just state } } } = do
  Console.log $ unsafeCoerce state
  maybe (pure unit) (send self) $ SetProduct <$> state.product
  maybe (pure unit) (send self) $ SetUser <$> state.user
  maybe (pure unit) (send self) $ SetPayment <$> state.payment
didMount _ = pure unit

initialState :: State
initialState =
  { product: Nothing
  , user: Nothing
  , payment: Nothing
  }

render :: Self -> JSX
render self =
  DOM.div
    { className: "clearfix"
    , children:
        [ DOM.div
            { className: "col-12 center"
            , children:
                [ DOM.h2_ [ DOM.text "Din prenumeration" ]
                , DOM.p_ [ DOM.text "Granska uppgifterna nedan före du fortsätter" ] ]
            }
        , confirmButton
        , DOM.div
            { className: "clearfix mt3"
            , children:
                [ column
                    productName
                    "Läspaket"
                    $ mkList
                      [ Tuple "Papperstidningen" $ DOM.text "Tryckta tidningen måndag-söndag"
                      , Tuple "HBL365 nyhetsapp" $ DOM.text "e-tidning, nyhetsflöde, pushnotiser"
                      , Tuple "eHBL" $ DOM.text "e-tidningen mobil, surfplatta och dator"
                      ]
                , column
                    ""
                    "Kontaktuppgifter"
                    $ mkList
                      [ Tuple "Namn" $ DOM.text $ foldMap customerName self.state.user
                      , Tuple "E-post" $ DOM.text $ foldMap _.email self.state.user
                      , Tuple "Adress" $ foldMap customerAddress $ self.state.user >>= (toMaybe <<< _.address)
                      , Tuple "Kundnummer" $ DOM.text $ foldMap _.cusno self.state.user
                      ]
                , foldMap payment self.state.payment
                ]
            }
        ]
    }
  where
    payment :: Payment -> JSX
    payment p =
      column
        ""
        "Prenumeration"
        $ mkList
          [ Tuple "Startdatum" $ DOM.text $ fold p.startDate
          , Tuple "Betalningssätt" $ DOM.text $ fold p.paymentMethod
          , foldMap (\price -> Tuple "Pris" $ DOM.text $ show price <> " €") p.price
          , foldMap (\extraCost -> Tuple "Tilläggsavgift" $ DOM.text $ "+ " <> show extraCost <> " €") p.surcharge
          , foldMap (\total -> Tuple "Totalt" $ DOM.strong_ [ DOM.text $ show total <> " €" ]) totalPrice
          ]
      where
        totalPrice =
          add <$> p.price <*> p.surcharge

    productName :: String
    productName = foldMap _.name self.state.product

    customerName :: Persona.User -> String
    customerName { firstName, lastName } =
      fname <> " " <> lname
      where
        fname = fold $ toMaybe firstName
        lname = fold $ toMaybe lastName

    customerAddress :: Persona.Address -> JSX
    customerAddress { streetAddress, zipCode, city, countryCode } =
      joinAddress $ catMaybes addressArray
      where
        addressArray =
          [ Just streetAddress
          , toMaybe zipCode
          , toMaybe city
          , Just countryCode
          ]

    joinAddress :: Array String -> JSX
    joinAddress address =
      DOM.ul_ $ map li address
      where
        li item = DOM.li_ [ DOM.text item ]


confirmButton :: JSX
confirmButton =
    DOM.div
    { className: "flex justify-center mt2"
    , children: [ link ]
    }
  where
    link =
      element
        Router.link
          { to: { pathname: "/order-successful", state: {} }
          , children: [ button ]
          , className: "prenumerera--button-link center col-2"
          }
    button = DOM.span_ [ DOM.text "Godkänn" ]

update :: Self -> Action -> StateUpdate Props State Action
update self = case _ of
  SetProduct p ->
    Update self.state { product = Just p }
  SetUser u ->
    Update self.state { user = Just u }
  SetPayment p ->
    Update self.state { payment = Just p }

column :: String -> String -> JSX -> JSX
column header description list =
  DOM.div
    { className: "col col-4 prenumerera--confirm-summary"
    , children:
        [ DOM.div
            { className: "prenumerera--confirm-summary-title"
            , children:
                [ DOM.text description ]
            }
        , DOM.strong_ [ DOM.text header ]
        , list
        ]
    }

mkList :: Array (Tuple String JSX) -> JSX
mkList listItems =
  DOM.div
    { className: ""
    , children: map list listItems
    }
  where
    list (Tuple title content) =
      DOM.div
        { className: ""
        , children:
            [ DOM.div
                { className: "clearfix mt2 mb2"
                , children:
                    [ row $ DOM.strong_ [ DOM.text title ]
                    , row content
                    ]
                }
            ]
        }
    row child =
      DOM.div
        { className: "col col-12"
        , children: [ child ]
        }
