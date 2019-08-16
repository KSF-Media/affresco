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
import React.Basic (JSX, StateUpdate(..), make, runUpdate)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.Events as Event

foreign import icons ::
  { signOut :: String
  , phone :: String
  }

type Self = React.Self Props State

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

fromJSProps :: JSProps -> Props
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
        _      -> KSF

type State =
  { collapsedNavVisibility :: Visibility  }

initialState :: State
initialState = { collapsedNavVisibility: Hidden }

jsComponent :: React.ReactComponent JSProps
jsComponent = React.toReactComponent fromJSProps component { render }

component :: React.Component Props
component = React.createComponent "Navbar"

navbar :: Props -> JSX
navbar = make component
  { initialState, render }

render :: Self -> JSX
render self@{ props, state } = nav self
  -- View.navbar
  --   { onLogout:
  --       if isJust props.loggedInUser
  --       then Just do
  --            props.logout
  --            send self (CollapsedNavVisibility Hidden)
  --       else Nothing
  --   , paperInfo
  --   , toggleCollapsedNav:
  --       send self (CollapsedNavVisibility $ negateVisibility state.collapsedNavVisibility)
  --   , collapsedNav:
  --       \items ->
  --         Collapsed.collapsed
  --           { visibility: state.collapsedNavVisibility
  --           , navItems: items
  --           }
  --   }
  -- where
  --   paperInfo =
  --     { logo: paperLogoUrl props.paper
  --     , email: paperEmail props.paper
  --     , phoneNumber: paperPhoneNumber props.paper
  --     }

nav :: Self -> JSX
nav self@{ props, state } =
  DOM.div
    { className: "nav--nav-container"
    , children:
        [ paperLogo props.paper
        , needHelp props.paper
        , signOutButton props.logout
        ]
    }

paperLogo :: Paper -> JSX
paperLogo paper =
  DOM.img { className: "nav--paper-logo", src: paperLogoUrl paper }

needHelp :: Paper -> JSX
needHelp paper =
  DOM.div
    { className: "nav--need-help"
    , children:
        [ DOM.strong_ [ DOM.text "Behöver du hjälp?" ]
        , DOM.div_ [ formatMailtoAnchorTag $ paperEmail paper ]
        ]
    }
    where
      formatMailtoAnchorTag :: String -> JSX
      formatMailtoAnchorTag email = DOM.a { href: "mailto:" <> email, children: [ DOM.text email ] }

signOutButton :: (Effect Unit) -> JSX
signOutButton logout =
  DOM.div
    { className: "nav--logout-button"
    , onClick: Event.handler_ logout
    , children:
        [ DOM.img
            { className: ""
            , src: icons.signOut
            }
        , DOM.div
            { className: ""
            , children: [ DOM.text "Logga ut" ]
            }
        ]
    }

update :: Self -> Action -> StateUpdate Props State
update self = case _ of
  CollapsedNavVisibility v ->
    Update self.state { collapsedNavVisibility = v }

send :: Self -> Action -> Effect Unit
send = runUpdate update

data Paper = HBL | ON | VN | LS | HTHL | KSF
derive instance eqPaper :: Eq Paper

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
