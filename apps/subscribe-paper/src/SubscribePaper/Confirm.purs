module SubscribePaper.Confirm where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Class.Console as Console
import Persona as Persona
import React.Basic (JSX, StateUpdate(..), element, make)
import React.Basic as React
import React.Basic.DOM as DOM
import Router (Location)
import Router as Router
import SubscribePaper.SubscribePaper (Product)
import Unsafe.Coerce (unsafeCoerce)

type Self = React.Self Props State Action

type Props = { location ::  Location (Maybe LocationState) }

type JSProps = { location :: Location (Nullable LocationJsState) }

type State = LocationState

data Action =
  SetProduct Product

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
didMount self =
  Console.log $ unsafeCoerce self.props

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
        ]
    }

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
          , className: "subscribe-paper--button-link center col-2"
          }
    button = DOM.span_ [ DOM.text "Godkänn" ]

update :: Self -> Action -> StateUpdate Props State Action
update self = case _ of
  SetProduct p ->
    Update self.state { product = Just p }
