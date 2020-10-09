module Prenumerera.ProductSelect where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Foreign (Foreign, unsafeFromForeign)
import KSF.Navbar.Component (Paper(..))
import React.Basic (JSX, StateUpdate(..), element, make, runUpdate)
import React.Basic as React
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)
import React.Basic.Router (Match)
import React.Basic.Router as Router
import Prenumerera.Package as Package
import Prenumerera.Prenumerera (Product, Package)
import Prenumerera.User as User

foreign import images :: { hblTotal :: String }

type Self = React.Self Props State
type Props = { paper :: Paper, match :: Maybe Match, onClick :: EventHandler }
type State =
  { paper :: Paper
  }

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
jsComponent :: React.ReactComponent JSProps
jsComponent =
  React.toReactComponent fromJsProps component { initialState, render }

fromJsProps :: JSProps -> Props
fromJsProps jsProps =
  { paper: toPaper jsProps.paper, match: jsMatch jsProps.match, onClick: jsProps.onClick }
  where
    toPaper paperString =
      case paperString of
        "HBL"  -> HBL
        "ON"   -> ON
        "VN"   -> VN
        _      -> KSF
    jsMatch match =
      Just
        { params: unsafeFromForeign match.params
        , isExact: match.isExact
        , path: match.path
        , url: match.url
        }

productSelect :: Props -> JSX
productSelect = make component { initialState, render }

initialState :: State
initialState = { paper: HBL }

update :: Self -> Action -> StateUpdate Props State
update self =
  case _ of
    ChangePaper paper ->
      Update self.state { paper = paper }

send :: Self -> Action -> Effect Unit
send = runUpdate update

render :: Self -> JSX
render self@{ props, state } =
  DOM.div_
    [ DOM.h2
        { className: "prenumerera--title center"
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
    , mkPackage self
    ]
    where
      choosePaper :: JSX
      choosePaper =
        DOM.div
        { className: "prenumerera--choose-paper pt2 pb2"
        , children:
            [ DOM.span_ [ DOM.text "Välj tidning:" ]
            , paperLink HBL "Hufvudstadsbladet"
            , paperLink VN "Västra Nyland"
            , paperLink ON "Östnyland"
            ]
        }

      paperLink :: Paper -> String -> JSX
      paperLink paper description =
        DOM.a
        { className: "m2" <> " " <> path <> " " <> active
        , href: "/#/" <> path
        , children: [ DOM.text description ]
        , onClick: capture_ (send self $ ChangePaper paper)
        }
        where
          active = if state.paper == paper then "active" else ""
          path = paperToString paper

defaultPackage :: Package
defaultPackage =
  { name: "HBL Total"
  , days: "måndag till söndag"
  , price: 37.90
  , image: ""
  , description: "7 dagar med papper"
  , checklist: defaultPackageChecklist
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

mkPackage :: Self -> JSX
mkPackage self =
  Package.package
    { package: defaultPackage
    , image: images.hblTotal
    , button: Just $ buyNowButton ("/buy/" <> defaultPackage.name) (Just { name: defaultPackage.name, price: defaultPackage.price })
    }

buyNowButton :: String -> (Maybe Product) -> JSX
buyNowButton linkTo product =
  DOM.div
    { className: "flex justify-center"
    , children:
        [ element
            Router.link
              { to: { pathname: linkTo, state: userState }
              , children: [ button ]
              , className: "prenumerera--button-link"
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
paperToString KSF  = "ksf"
