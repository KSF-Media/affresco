module KSF.Navbar.Component where

import Prelude

import Data.Foldable (foldMap)
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
render self@{ props, state } = nav self <> collapsedNav self

nav :: Self -> JSX
nav self@{ props, state } =
  DOM.div
    { className: "nav--nav-container"
    , children:
        [ paperLogo props.paper
        , needHelp props.paper
        , foldMap signOutButton (onLogout self =<< props.loggedInUser)
        ]
    }

collapsedNav :: Self -> JSX
collapsedNav self@{ props, state } =
  DOM.div
    { className: "nav--collapsed-container"
    , children:
        [ paperLogo props.paper
        , hamburgerButton self
       -- , needHelp props.paper
        --, foldMap signOutButton (onLogout self =<< props.loggedInUser)
        ]
    }

onLogout :: Self -> Persona.User -> Maybe (Effect Unit)
onLogout self _user = Just do
  self.props.logout
  self.setState _ { collapsedNavVisibility = Hidden }

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
        [ DOM.img { src: icons.signOut }
        , DOM.div_ [ DOM.text "Logga ut" ]
        ]
    }

hamburgerButton :: Self -> JSX
hamburgerButton self =
  DOM.div
    { className: "col mr3 nav--hamburger-button"
    , onClick: Event.handler_ $ self.setState _ { collapsedNavVisibility = negateVisibility self.state.collapsedNavVisibility }
    , children:
        [ hamburgerStripe
        , hamburgerStripe
        , hamburgerStripe
        ]
    }
  where
    hamburgerStripe = DOM.div { className: "nav--hamburger-stripe" }

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
