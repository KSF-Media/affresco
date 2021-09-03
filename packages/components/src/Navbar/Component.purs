module KSF.Navbar.Component where

import Prelude

import Data.Array (replicate)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Effect (Effect)
import KSF.Icons (papers)
import KSF.Navbar.Collapsed.Component (Visibility(..), negateVisibility)
import KSF.Navbar.Collapsed.Component as Collapsed
import KSF.Paper (Paper(..))
import KSF.User (User)
import React.Basic.Classic (JSX, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.Events as Event

foreign import icons ::
  { signOut :: String
  , phone :: String
  }

type Self = React.Self Props State

type Props =
  { paper :: Paper
  , specialHelp :: Maybe JSX
  , activeUser :: Maybe User
  , logout :: Effect Unit
  }

type JSProps =
  { paperCode :: String
  , specialHelp :: Nullable JSX
  , activeUser :: Nullable User
  , onLogout :: Effect Unit
  }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { paper
  , specialHelp: Nullable.toMaybe jsProps.specialHelp
  , activeUser: Nullable.toMaybe jsProps.activeUser
  , logout: jsProps.onLogout
  }
  where
    paper =
      case String.toUpper jsProps.paperCode of
        "HBL"    -> HBL
        "ON"     -> ON
        "VN"     -> VN
        "JUNIOR" -> JUNIOR
        _        -> KSF

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
render self =
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
fullNav self@{ props } =
  DOM.div
    { className: "nav--nav-container"
    , children:
        [ paperLogo props.paper
        , fromMaybe needHelp props.specialHelp
        , showUser self
        , logoutButton self
        ]
    }

showUser :: Self -> JSX
showUser { props: { activeUser } } =
  foldMap displayUser activeUser
  where
    displayUser user =
      DOM.div
        { className: "nav--display-user"
        , children:
            [ DOM.img
                { src: "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/icons/fa-user-circle-o.svg"
                , alt: "User"
                }
            , DOM.text $ fromMaybe user.email $ Nullable.toMaybe user.firstName
            ]
        }

logoutButton :: Self -> JSX
logoutButton self@{ props: { activeUser } } =
  foldMap button activeUser
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
hamburgerNav self@{ props } =
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
    , navItems: [ logoutButton self, needHelp ]
    }

paperLogo :: Paper -> JSX
paperLogo paper =
  DOM.img { className: "nav--paper-logo", src: paperLogoUrl paper }

needHelp :: JSX
needHelp =
  DOM.div
    { className: "nav--logout-limpet"
    , children:
        [ DOM.strong_ [ DOM.text "Behöver du hjälp?" ]
        , DOM.div_ [ formatMailtoAnchorTag "pren@ksfmedia.fi" ]
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

paperLogoUrl :: Paper -> String
paperLogoUrl paper =
  case paper of
    HBL    -> papers.hbl
    ON     -> papers.on
    VN     -> papers.vn
    KSF    -> papers.ksf
    JUNIOR -> papers.junior
