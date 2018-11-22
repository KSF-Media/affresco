module SubscribePaper.ProductSelect where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import KSF.Navbar.Component (Paper(..))
import Persona as Persona
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events as Event

foreign import images :: { hblTotal :: String }

type Props = { paper :: Paper }
type State =
  { paper :: Paper
  }

type SetState = (State -> State) -> Effect Unit

component :: React.Component Props
component = React.component { displayName: "ProductSelect", render, initialState: { paper: HBL }, receiveProps }

receiveProps _ = pure unit

render :: forall r. { props :: Props, state :: State, setState :: SetState | r } -> JSX
render { props, state, setState } =
  DOM.div_
    [ DOM.h2
        { className: "subscribe-paper--title center"
        , children:
          [ DOM.span
                        { className: paperToString state.paper
                        , children: [ DOM.text "Kvalitetsjournalistik" ]
                        }
          , DOM.text " när, var och hur du vill"
          ]
        }
    , DOM.h3_ [ DOM.text "Välj det paket som passar dig bäst!" ]
    , choosePaper
    , mkPackage $
                { name: "HBL Total"
                , days: "måndag till söndag"
                , price: 37.90
                , image: ""
                , description: "7 dagar med papper"
                , checklist: defaultPackageChecklist
                }
    ]
    where
      choosePaper :: JSX
      choosePaper =
        DOM.div
        { className: "subscribe-paper--choose-paper pt2 pb2"
        , children:
            [ DOM.span_ [ DOM.text "Välj tidning:" ]
            , paperLink HBL "Hufvudstadsbladet"
            , paperLink VN "Västra Nyland"
            , paperLink ON "Östnyland"
            , paperLink LS "Loviisan Sanomat"
            , paperLink HTHL "Hangötidningen - Hangonlehti"
            ]
        }

      paperLink :: Paper -> String -> JSX
      paperLink paper description =
        DOM.a
        { className: "m2" <> " " <> path <> " " <> active
        , href: "/#/" <> path
        , children: [ DOM.text description ]
        , onClick: Event.handler_ $ setState \s -> s { paper = paper }
        }
        where
          active = if state.paper == paper then "active" else ""
          path = paperToString paper


type Package =
  { name        :: String
  , price       :: Number
  , days        :: String
  , image       :: String
  , description :: String
  , checklist   :: Array { title :: String, content :: String }
  }

defaultPackageChecklist :: Array { title :: String, content :: String }
defaultPackageChecklist =
  [ { title: "Papperstidningen"
    , content: "Tryckta tidningen måndag-söndag"
    }
  , { title: "HBL365 nyhetsapp"
    , content: "e-tidning, nyhetsflöde, pushnotiser"
    }
  , { title: "eHBL"
    , content: "e-tidningen mobil, surfplatta och dator"
    }
  , { title: "Alla artiklar"
    , content: "hbl.fi, vastranyland.fi, ostnyland.fi"
    }
  , { title: "Digitalt månadsbrev"
    , content: "Nyheter & förmåner"
    }
  ]

mkPackage :: Package -> JSX
mkPackage package =
  DOM.div
    { className: "subscribe-paper--package col-2 center clearfix"
    , children:
        [ DOM.h2_ [ DOM.text package.name ]
        , DOM.div
            { className: "subscribe-paper--package-days p2"
            , children: [ DOM.strong_ [ DOM.text package.days ] ]
            }
        , DOM.div
            { className: "subscribe-paper--package-description pt1"
            , children:
                [ DOM.img
                    { src: images.hblTotal }
                ]
            }
        , buyNowButton
        , mkChecklist package.checklist
        ]
    }

mkChecklist :: Array { title :: String, content :: String } -> JSX
mkChecklist checklist =
  DOM.ul
    { className: "subscribe-paper--package-check-list"
    , children: map listItem checklist
    }
  where
    listItem { title, content } =
      DOM.li_
        [ DOM.p_
            [ DOM.strong_ [ DOM.text title ]
            , DOM.br {}
            , DOM.text content ]
        ]

buyNowButton :: JSX
buyNowButton =
  DOM.div
    { className: "subscribe-paper--buy-now flex justify-center"
    , children:
        [ DOM.span_ [ DOM.text "Köp nu" ] ]
    }

paperToString :: Paper -> String
paperToString HBL  = "hbl"
paperToString ON   = "on"
paperToString VN   = "vn"
paperToString LS   = "ls"
paperToString HTHL = "hthl"
paperToString KSF  = "ksf"
