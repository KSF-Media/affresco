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

stages :: Array StageDescription
stages =
  [ { num: "1"
    , desc: "Skapa ditt konto"
    , stage: Login
    }
  , { num: "2"
    , desc: "Godkänn villkor"
    , stage: Accept
    }
  , { num: "3"
    , desc: "Betalningsuppgifter"
    , stage: Payment
    }
  , { num: "4"
    , desc: "Bekräftelse"
    , stage: Success
    }
  ]

render :: Progress -> JSX
render progress =
  DOM.div
    { className: "container"
    , children:
        [ DOM.div
            { className: "row"
            , children:
                [ DOM.ul
                    { id: "progressIndicator"
                    , className: "ksf-progress-bar"
                    , children: map renderStage stages
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
