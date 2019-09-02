module KSF.Navbar.Component where

import Prelude

import Data.Array (replicate)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Effect (Effect)
import KSF.Icons (papers)
import KSF.Navbar.Collapsed.Component (Visibility(..), negateVisibility)
import KSF.Navbar.Collapsed.Component as Collapsed
import Persona as Persona
import React.Basic (JSX, make)
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
render self@{ props, state } =
  DOM.div
    { className: "nav--navbars"
    , children:
        [ fullNav self
        , hamburgerNav self
        , collapsedNav self
        ]
    }

-- | Full width navbar
fullNav :: Self -> JSX
fullNav self@{ props, state } =
  DOM.div
    { className: "nav--nav-container"
    , children:
        [ paperLogo props.paper
        , needHelp props.paper
        , logoutButton self
        ]
    }

logoutButton :: Self -> JSX
logoutButton self@{ props: { logout, loggedInUser } } =
  foldMap button loggedInUser
  where
    button _user =
      DOM.div
        { className: "nav--logout-button"
        , onClick: Event.handler_ onLogout
        , children:
            [ DOM.img { src: icons.signOut }
            , DOM.div_ [ DOM.text "Logga ut" ]
            ]
        }
    onLogout = do
      self.props.logout
      self.setState _ { collapsedNavVisibility = Hidden }

-- | Narrow navbar with hamburger button
hamburgerNav :: Self -> JSX
hamburgerNav self@{ props, state } =
  DOM.div
    { className: "nav--hamburger-container"
    , children:
        [ paperLogo props.paper
        , hamburgerButton self
        ]
    }

-- | Items of hamburger nav
collapsedNav :: Self -> JSX
collapsedNav self =
  Collapsed.collapsed
    { visibility: self.state.collapsedNavVisibility
    , navItems: [ logoutButton self, needHelp self.props.paper ]
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

hamburgerButton :: Self -> JSX
hamburgerButton self =
  DOM.div
    { className: "nav--hamburger-button"
    , onClick: Event.handler_ $ self.setState _ { collapsedNavVisibility = negateVisibility self.state.collapsedNavVisibility }
    , children: replicate 3 hamburgerStripe
    }
  where
    hamburgerStripe = DOM.div { className: "nav--hamburger-stripe" }

data Paper = HBL | ON | VN | HTHL | KSF
derive instance eqPaper :: Eq Paper

paperLogoUrl :: Paper -> String
paperLogoUrl paper =
  case paper of
    HBL  -> papers.hbl
    ON   -> papers.on
    VN   -> papers.vn
    HTHL -> papers.hthl
    KSF  -> papers.ksf

paperEmail :: Paper -> String
paperEmail paper =
  case paper of
    HBL  -> "pren@hbl.fi"
    ON   -> "pren@ostnyland.fi"
    VN   -> "pren@vastranyland.fi"
    HTHL -> "hangonlehti@hangotidningen.fi"
    KSF  -> paperEmail HBL
