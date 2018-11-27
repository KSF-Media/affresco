module SubscribePaper.ConfirmPurchase where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign (Foreign)
import Persona as Persona
import React.Basic (JSX, StateUpdate(..), capture_, element, make)
import React.Basic as React
import React.Basic.DOM as DOM
import Router (Match)
import Router as Router
import SubscribePaper.SubscribePaper (Product)
import Unsafe.Coerce (unsafeCoerce)

type Self = React.Self Props State Action
type Props = { match :: Maybe Match }
type State =
  { user :: Maybe Persona.User
  , product :: Product
  , termsAccepted :: Boolean
  }

data Action =
  AcceptTerms

component :: React.Component Props
component = React.createComponent "ConfirmPurchase"

type JSProps =
  { match ::
      { params  :: Foreign
      , isExact :: Boolean
      , path    :: String
      , url     :: String
      }
  }

jsComponent :: React.ReactComponent JSProps
jsComponent =
  React.toReactComponent (\jsProps -> { match: Just jsProps.match }) component { initialState, render, didMount, update }

confirmPurchase :: Props -> JSX
confirmPurchase = make component { initialState, render, didMount, update }

didMount :: Self -> Effect Unit
didMount self =
  Console.log $ unsafeCoerce self.props

initialState :: State
initialState =
  { user: Nothing
  , product: { name: "", price: 0.0 }
  , termsAccepted: false
  }

update :: Self -> Action -> StateUpdate Props State Action
update self = case _ of
  AcceptTerms ->
    Update self.state { termsAccepted = not self.state.termsAccepted }

render :: Self -> JSX
render self =
    DOM.div
      { className: "clearfix flex justify-center"
      , children:
          [ DOM.div
              { className: "center col-4"
              , children:
                  [ DOM.h2_ [ DOM.text "Villkor och betalningssätt" ]
                  , DOM.p_ [ DOM.text $ "Välj betalningssätt och faktureringsperiod för din produkt " <> "\"" <> self.state.product.name <> "\"" ]
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
                      [ { value: "bill", text: "Faktura" } ]
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
                  , price self.state.product
                  , acceptTerms self
                  ]
              }
          ]
      }

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
                    , onChange: capture_ self AcceptTerms
                    , className: "subscribe-paper--accept-terms"
                    }
                , DOM.text "Jag har läst och godkänner KSF Medias bruksvillkor"
                ]
            }
        , DOM.div
            { className: ""
            , children:
                [ continueButton disabled ]
            }
        ]
    }
  where
    disabled = if self.state.termsAccepted then "" else "disabled"

continueButton :: String -> JSX
continueButton disabled =
  DOM.div
    { className: "subscribe-paper--continue flex justify-center " <> disabled
    , children:
        [ DOM.span_ [ link ] ]
    }
  where
    link = element Router.link { to: "/order-successful", children: [ "Fortsätt" ] }


price :: Product -> JSX
price product =
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
        , DOM.div
            { className: "col col-12 left-align"
            , children: [ DOM.text $ "+ " <> show extraCost <> " € fakturerungstillägg per pappersfaktura" ]
            }
        , DOM.div
            { className: "col col-12 left-align"
            , children: [ DOM.hr { className: "subscribe-paper--break" } ]
            }
        , DOM.div
            { className: "col col-12 left-align"
            , children:
                [ DOM.strong_ [ DOM.text $ "Totalpris: " <> show (extraCost + product.price) <> " €" ] ]
            }
        ]
    }
  where
    extraCost = 5.0

select :: Array { value :: String, text :: String } -> JSX
select values =
  DOM.div
    { className: "col col-12 mt1"
    , children:
       [ DOM.select
           { children: map option values
           , className: "subscribe-paper--payment-select"
           }
       ]
    }
  where
    option { value, text } = DOM.option { value, children: [ DOM.text text ] }
