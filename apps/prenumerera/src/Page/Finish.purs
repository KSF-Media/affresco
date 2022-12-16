module Prenumerera.Page.Finish where

import Prelude

import Data.Either (Either)
import Effect.Now as Now
import KSF.User (User)
import Prenumerera.Package.Description (Description)
import Prenumerera.Summary as Summary
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React

type Props =
  { user :: User
  , description :: Description
  , orderDetails :: Either Summary.GiftState Summary.PaymentDetails
  }

component :: Component Props
component = do
  today <- Now.nowDate
  React.component "Finish" \ { user, description, orderDetails } -> do
    let summary = Summary.render today user description orderDetails
    pure $ render summary

render :: JSX -> JSX
render summary =
  DOM.div
    { className: "container ksf-block confirm"
    , children:
        [ DOM.div
            { className: "row"
            , children:
                [ DOM.div
                    { className: "confirm-greeting"
                    , children:
                        [ DOM.h2_ [ DOM.text "Tack för din prenumeration!" ]
                        , DOM.p_ [ DOM.text "Vi har skickat en prenumerationsbekräftelse och instruktioner till hur du tar i bruk våra digitala tjänster till din e-post. (Kolla vid behov också i skräppostmappen...)" ]
                        ]
                    }
                , summary
                , DOM.a
                    { href: "/"
                    , children: [ DOM.text "Tillbaka till startsidan" ]
                    }
                ]
            }
        ]
    }
