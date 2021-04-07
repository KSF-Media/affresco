module Prenumerera.PaymentSelect where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign (Foreign)
import KSF.User (User)
import Prenumerera.Confirm as Confirm
import Prenumerera.Prenumerera (Product)
import React.Basic.Classic (JSX, StateUpdate(..), make, runUpdate)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (EventHandler, handler)
import React.Basic.Router (Match, Location)
import React.Basic.Router as Router
import Unsafe.Coerce (unsafeCoerce)

type Self = React.Self Props State
type Props = { match :: Maybe Match, location :: Location (Maybe LocationState) }
type State =
  { user :: Maybe User
  , product :: Maybe Product
  , termsAccepted :: Boolean
  , payment :: Payment
  }

type Payment =
  { paymentMethod :: Maybe String
  , surcharge :: Maybe Number
  }

type LocationState   = { product :: Maybe Product, user :: Maybe User }
type LocationJsState = { product :: Nullable Product, user :: Nullable User }

data Action =
  AcceptTerms
  | SetUser User
  | SetProduct Product
  | PaymentMethod String

component :: React.Component Props
component = React.createComponent "ConfirmPurchase"

type JSProps =
  { match ::
      { params  :: Foreign
      , isExact :: Boolean
      , path    :: String
      , url     :: String
      }
  , location :: Location (Nullable LocationJsState)
  }

fromJsProps :: JSProps -> Props
fromJsProps { match, location: { key, pathname, search, hash, state } } =
  { match: Just match
  , location:
      { key
      , pathname
      , search
      , hash
      , state: toNullable $ (convertState <<< toMaybe) <$> toMaybe state
      }
  }
  where
    convertState :: Maybe LocationJsState -> Maybe LocationState
    convertState (Just s) =
      Just
        { product: toMaybe s.product
        , user: toMaybe s.user
        }
    convertState _ = Nothing

jsComponent :: React.ReactComponent JSProps
jsComponent =
  React.toReactComponent fromJsProps component { initialState, render, didMount, didUpdate }

paymentSelect :: Props -> JSX
paymentSelect props = make component { initialState, render, didMount, didUpdate } props

didMount :: Self -> Effect Unit
didMount self = do
  let state = join $ toMaybe self.props.location.state
  updateState (SetUser <$> (_.user =<< state))
  updateState (SetProduct <$> (_.product =<< state))
  send self (PaymentMethod "bill")
  where
    updateState maybeAction =
      maybe (pure unit) (send self) maybeAction

didUpdate :: Self -> { prevProps :: Props, prevState :: State } -> Effect Unit
didUpdate self _ = do
 Console.log "did update payment select"
 Console.log $ unsafeCoerce self.state

initialState :: State
initialState =
  { user: Nothing
  , product: Nothing
  , termsAccepted: false
  , payment: { paymentMethod: Nothing, surcharge: Just 5.0 }
  }

update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  AcceptTerms ->
    Update self.state { termsAccepted = not self.state.termsAccepted }
  SetUser u ->
    Update self.state { user = Just u }
  SetProduct p ->
    Update self.state { product = Just p }
  PaymentMethod pm ->
    Update self.state { payment { paymentMethod = Just pm } }

send :: Self -> Action -> Effect Unit
send = runUpdate update

monitor_ :: Self -> Action -> EventHandler
monitor_ self action = handler identity \_ -> send self action

render :: Self -> JSX
render self =
    DOM.div
      { className: "clearfix flex justify-center"
      , children:
          [ DOM.div
              { className: "center col-4"
              , children:
                  [ DOM.h2_ [ DOM.text "Villkor och betalningssätt" ]
                  , DOM.p_ [ DOM.text $ "Välj betalningssätt och faktureringsperiod för din produkt " <> "\"" <> foldMap productName self.state.product <> "\"" ]
                  , DOM.p_ [ DOM.text "Prenumerationstyp: Fortlöpande" ]
                  , DOM.div
                      { className: "col col-12 mt1"
                      , children:
                          [ DOM.span
                               { className: "col col-12 mt1"
                               , children: [ DOM.strong_ [ DOM.text "Betalningssätt:" ] ]
                               }
                          ]
                      }
                  , select
                      [ { value: "bill", text: "Faktura" }
                      , { value: "creditCard", text: "Kreditkort" }
                      ]
                      self
                  , DOM.div
                      { className: "col col-12 mt1"
                      , children:
                          [ DOM.span
                              { className: "col col-12 mt1"
                              , children: [ DOM.strong_ [ DOM.text "Faktureringsperiod:" ] ]
                              }
                          ]
                      }
                  , select
                      [ { value: "1", text: "1 månader" } ]
                      self
                  , foldMap (price self) self.state.product
                  , acceptTerms self
                  ]
              }
          ]
      }
  where
    productName :: Product -> String
    productName product = product.name

acceptTerms :: Self -> JSX
acceptTerms self =
  DOM.div
    { className: "col col-12 mt3"
    , children:
        [ DOM.div
            { className: ""
            , children:
                [ DOM.input
                    { type: "checkbox"
                    , onChange: monitor_ self AcceptTerms
                    , className: "prenumerera--accept-terms"
                    }
                , DOM.text "Jag har läst och godkänner KSF Medias bruksvillkor"
                ]
            }
        , DOM.div
            { className: ""
            , children:
                [ continueButton self ]
            }
        ]
    }

continueButton :: Self -> JSX
continueButton self =
  DOM.div
    { className: "flex justify-center"
    , children: [ link ]
    }
  where
    link =
      Router.link
        { to: { pathname: "/confirm", state }
        , children: [ button ]
        , className: "prenumerera--button-link " <> disabled
        }
    state :: Confirm.LocationJsState
    state =
      { user: toNullable self.state.user
      , product: toNullable self.state.product
      , payment:
          toNullable $
            Just { startDate: toNullable $ Just "1.1.2020"
                 , paymentMethod: toNullable $ self.state.payment.paymentMethod
                 , price: toNullable $ _.price <$> self.state.product
                 , surcharge: toNullable $ self.state.payment.surcharge
                 }
      }
    button = DOM.span_ [ DOM.text "Fortsätt" ]
    disabled = if self.state.termsAccepted then "" else "disabled"

price :: Self -> Product -> JSX
price self product =
  DOM.div
    { className: "col mt3"
    , children:
        [ DOM.div
            { className: "col col-12 left-align"
            , children: [ DOM.strong_ [ DOM.text "Ditt pris" ] ]
            }
        , DOM.div
            { className: "col col-12 left-align"
            , children: [ DOM.strong_ [ DOM.text $ show product.price <> " €" ] ]
            }
        , foldMap
            (\cost -> DOM.div
                       { className: "col col-12 left-align"
                       , children: [ DOM.text $ "+ " <> show cost <> " € fakturerungstillägg per pappersfaktura" ]
                       })
            self.state.payment.surcharge
        , DOM.div
            { className: "col col-12 left-align"
            , children: [ DOM.hr { className: "prenumerera--break" } ]
            }
        , DOM.div
            { className: "col col-12 left-align"
            , children:
                [ DOM.strong_ [ DOM.text $ "Totalpris: " <> show (extraCost + product.price) <> " €" ] ]
            }
        ]
    }
  where
    extraCost = fromMaybe 0.0 self.state.payment.surcharge

select :: Array { value :: String, text :: String } -> Self -> JSX
select values self =
  DOM.div
    { className: "col col-12 mt1"
    , children:
       [ DOM.select
           { children: map option values
           , className: "prenumerera--payment-select"
           , onChange: handler targetValue $ updatePaymentMethod
           }
       ]
    }
  where
    updatePaymentMethod :: Maybe String -> Effect Unit
    updatePaymentMethod (Just method) = send self $ PaymentMethod method
    updatePaymentMethod _ = pure unit

    option { value, text } = DOM.option { value, children: [ DOM.text text ] }
