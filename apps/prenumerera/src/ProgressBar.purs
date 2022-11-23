module Prenumerera.ProgressBar where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as DOM

data Progress = Login | Accept | Payment | Success

derive instance eqProgress :: Eq Progress
derive instance ordProgress :: Ord Progress

type StageDescription =
  { num :: String
  , desc :: String
  , stage :: Progress
  }

stages :: Boolean -> Array StageDescription
stages giftRedeem =
  [ { num: "1"
    , desc: "Skapa ditt konto"
    , stage: Login
    }
  , { num: "2"
    , desc: "Godkänn villkor"
    , stage: Accept
    }
  , { num: "3"
    , desc: if giftRedeem then "Förlossa" else "Betalningsuppgifter"
    , stage: Payment
    }
  , { num: "4"
    , desc: "Bekräftelse"
    , stage: Success
    }
  ]

render :: Progress -> Boolean -> JSX
render progress giftRedeem =
  DOM.div
    { className: "container"
    , children:
        [ DOM.div
            { className: "row"
            , children:
                [ DOM.ul
                    { id: "progressIndicator"
                    , className: "ksf-progress-bar"
                    , children: map renderStage $ stages giftRedeem
                    }
                ]
            }
        ]
    }
  where
    renderStage { num, desc, stage } =
      DOM.li
        { className: "col-xs-3" <>
            (case stage of
                Login -> " first"
                Success -> " last"
                _ -> "") <>
            (if progress == stage then " active" else "")
        , children:
            (if stage /= Login then [ DOM.span { className: "tail" } ] else []) <>
            [ DOM.span { className: "nr", children: [ DOM.text num ] }
            , DOM.span { className: "desc", children: [ DOM.text desc ] }
            ]
        }
