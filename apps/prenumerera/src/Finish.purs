module Prenumerera.Finish where

import Prelude

import Bottega.Models (PaymentMethod)
import KSF.User (User)
import Prenumerera.Package (PackageOffer)
import Prenumerera.PackageDescription (Description)
import Prenumerera.Summary as Summary
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React

type Props =
  { user :: User
  , description :: Description
  , offer :: PackageOffer
  , method :: PaymentMethod
  }

component :: Component Props
component = do
  React.component "Finish" \ { user, description, offer, method } -> do
    let summary = Summary.render user description offer method
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
                ]
            }
        ]
    }
