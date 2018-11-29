module KSF.Navbar.Component where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Effect (Effect)
import KSF.Icons (papers)
import KSF.Navbar.Collapsed.Component (Visibility(..), negateVisibility)
import KSF.Navbar.Collapsed.Component as Collapsed
import KSF.Navbar.View as View
import Persona as Persona
import React.Basic (JSX, StateUpdate(..), make, send)
import React.Basic as React

type Self = React.Self Props State Void

type Props =
  { paper :: Paper
  , loggedInUser :: Maybe Persona.User
  , logout :: Effect Unit
  }

type JSProps =
  { paperCode :: String
  , loggedInUser :: Nullable Persona.User
  , onLogout :: Effect Unit
  }

data Action =
  CollapsedNavVisibility Visibility

fromJSProps :: Partial => JSProps -> Props
fromJSProps jsProps =
  { paper
  , loggedInUser: Nullable.toMaybe jsProps.loggedInUser
  , logout: jsProps.onLogout
  }
  where
    paper =
      case String.toUpper jsProps.paperCode of
        "HBL"  -> HBL
        "ON"   -> ON
        "VN"   -> VN
        "LS"   -> LS
        "HTHL" -> HTHL
        "KSF"  -> KSF

type State =
  { collapsedNavVisibility :: Visibility  }

initialState :: State
initialState = { collapsedNavVisibility: Hidden }

jsComponent :: Partial => React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { initialState, render, update }

component :: React.Component Props
component = React.createComponent "Navbar"

navbar :: Props -> JSX
navbar = make component
  { initialState, render, update }

render :: Self -> JSX
render self@{ props, state } =
  View.navbar
    { onLogout:
        if isJust props.loggedInUser
        then Just do
             props.logout
             send self (CollapsedNavVisibility Hidden)
        else Nothing
    , paperInfo
    , toggleCollapsedNav:
        send self (CollapsedNavVisibility $ negateVisibility state.collapsedNavVisibility)
    , collapsedNav:
        \items ->
          Collapsed.collapsed
            { visibility: state.collapsedNavVisibility
            , navItems: items
            }
    }
  where
    paperInfo =
      { logo: paperLogoUrl props.paper
      , email: paperEmail props.paper
      , phoneNumber: paperPhoneNumber props.paper
      }

update :: Self -> Action -> StateUpdate Props State Action
update self = case _ of
  CollapsedNavVisibility v ->
    Update self.state { collapsedNavVisibility = v }

data Paper = HBL | ON | VN | LS | HTHL | KSF

paperLogoUrl :: Paper -> String
paperLogoUrl paper =
  case paper of
    HBL  -> papers.hbl
    ON   -> papers.on
    VN   -> papers.vn
    LS   -> papers.ls
    HTHL -> papers.hthl
    KSF  -> papers.ksf

paperEmail :: Paper -> String
paperEmail paper =
  case paper of
    HBL  -> "pren@hbl.fi"
    ON   -> "pren@ostnyland.fi"
    VN   -> "pren@vastranyland.fi"
    LS   -> "tilaukset@lovari.fi"
    HTHL -> "hangonlehti@hangotidningen.fi"
    KSF  -> paperEmail HBL

paperPhoneNumber :: Paper -> String
paperPhoneNumber paper =
  case paper of
    HBL  -> "09 125 3500"
    ON   -> "020 7569 650"
    VN   -> "019 222 866"
    LS   -> "019 532 701"
    HTHL -> "019 312 140"
    KSF  -> paperPhoneNumber HBL
