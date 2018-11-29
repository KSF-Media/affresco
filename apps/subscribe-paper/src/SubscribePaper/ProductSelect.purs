module SubscribePaper.ProductSelect where

import Prelude

import Data.Function.Uncurried (Fn0, mkFn0, runFn0)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign (Foreign, unsafeFromForeign)
import KSF.Navbar.Component (Paper(..))
import Persona (User)
import Persona as Persona
import Prim.Row (class Union)
import React.Basic (ComponentSpec, JSX, StateUpdate(..), capture_, element, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import React.Basic.Events as Event
import Router (Match)
import Router as Router
import SubscribePaper.SubscribePaper (Product)
import SubscribePaper.User as User

foreign import images :: { hblTotal :: String }

type Self = React.Self Props State Action
type Props = { paper :: Paper, match :: Maybe Match, onClick :: EventHandler }
type State =
  { paper :: Paper
  }

type SetState = (State -> State) -> Effect Unit

data Action =
  ChangePaper Paper

component :: React.Component Props
component = React.createComponent "ProductSelect"

type JSProps =
  { paper :: String
  , match ::
      { params :: Foreign
      , isExact :: Boolean
      , path :: String
      , url :: String
      }
  , onClick :: EventHandler
  }

-- | For javascript FFI needs
reactComponent :: React.ReactComponent JSProps
reactComponent =
  React.toReactComponent (\jsProps -> { paper: toPaper jsProps.paper, match: jsMatch jsProps.match, onClick: jsProps.onClick }) component { initialState, render, update }
  where
    toPaper paperString =
      case paperString of
        "HBL"  -> HBL
        "ON"   -> ON
        "VN"   -> VN
        "LS"   -> LS
        "HTHL" -> HTHL
        _      -> KSF
    jsMatch match =
        Just
          { params: unsafeFromForeign match.params
          , isExact: match.isExact
          , path: match.path
          , url: match.url
          }

productSelect :: Props -> JSX
productSelect = make component { initialState, render, update }

initialState :: State
initialState = { paper: HBL }

update :: Self -> Action -> StateUpdate Props State Action
update self =
  case _ of
    ChangePaper paper ->
      Update self.state { paper = paper }

render :: Self -> JSX
render self@{ props, state } =
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
    , mkPackage
                { name: "HBL Total"
                , days: "måndag till söndag"
                , price: 37.90
                , image: ""
                , description: "7 dagar med papper"
                , checklist: defaultPackageChecklist
                }
                self
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
        , onClick: capture_ self $ ChangePaper paper
          --Event.handler_ $ setState \s -> s { paper = paper }
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

mkPackage :: Package -> Self -> JSX
mkPackage package self =
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
        , buyNowButton ("/buy/" <> package.name) (Just { name: package.name, price: package.price })
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

buyNowButton :: String -> (Maybe Product) -> JSX
buyNowButton linkTo product =
  DOM.div
    { className: "flex justify-center"
    , children:
        [ element
            Router.link
              { to: { pathname: linkTo, state: userState }
              , children: [ button ]
              , className: "subscribe-paper--button-link"
              }
        ]
    }
  where
    button = DOM.span { children: [ DOM.text "Köp nu" ] }
    userState :: User.LocationJsState
    userState = { product: toNullable product }

paperToString :: Paper -> String
paperToString HBL  = "hbl"
paperToString ON   = "on"
paperToString VN   = "vn"
paperToString LS   = "ls"
paperToString HTHL = "hthl"
paperToString KSF  = "ksf"
