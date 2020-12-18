module KSF.Navbar.Component where

import Prelude

import Data.Array (replicate, mapMaybe)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
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
import React.Basic.Router as Router

foreign import icons ::
  { signOut :: String
  , phone :: String
  }

type Self = React.Self Props State

type Props =
  { paper :: Paper
  , adminMode :: Boolean
  , isPersonating :: Boolean
  , activeUser :: Maybe User
  , logout :: Effect Unit
  }

type JSProps =
  { paperCode :: String
  , adminMode :: Boolean
  , isPersonating :: Boolean
  , activeUser :: Nullable User
  , onLogout :: Effect Unit
  }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  { paper
  , adminMode: jsProps.adminMode
  , isPersonating: jsProps.isPersonating
  , activeUser: Nullable.toMaybe jsProps.activeUser
  , logout: jsProps.onLogout
  }
  where
    paper =
      case String.toUpper jsProps.paperCode of
        "HBL"  -> HBL
        "ON"   -> ON
        "VN"   -> VN
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
        , if not props.adminMode
            then needHelp props.paper
            else customerService props.isPersonating props.activeUser
        , logoutButton self
        ]
    }

logoutButton :: Self -> JSX
logoutButton self@{ props: { logout, activeUser } } =
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
    { className: "nav--logout-limpet"
    , children:
        [ DOM.strong_ [ DOM.text "Behöver du hjälp?" ]
        , DOM.div_ [ formatMailtoAnchorTag $ paperEmail paper ]
        ]
    }
    where
      formatMailtoAnchorTag :: String -> JSX
      formatMailtoAnchorTag email = DOM.a { href: "mailto:" <> email, children: [ DOM.text email ] }

customerService :: Boolean -> Maybe Persona.User -> JSX
customerService isPersonating activeUser = do
  DOM.div
    { className: "nav--logout-limpet"
    , children: [ Router.link
                    { to: { pathname: "/sök", state: {} }
                    , children: [ DOM.text "Sök kund" ]
                    , className: ""
                    }
                ] <> if isPersonating
                       then ( case activeUser of
                                 Just user -> [ DOM.div_ [ DOM.strong_ [ DOM.text "Aktiv kund" ] ]
                                              , userLink user
                                              ]
                                 Nothing -> []
                            )
                       else []
    }
    where
      userLink user =
        Router.link
          { to: { pathname: "/", state: {} }
          , children: [ DOM.text $ user.cusno <> " - " <>
                          ( String.joinWith " " $
                            mapMaybe toMaybe [ user.firstName, user.lastName ] )
                      ]
          , className: ""
          }

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
    HBL  -> papers.hbl
    ON   -> papers.on
    VN   -> papers.vn
    KSF  -> papers.ksf

paperEmail :: Paper -> String
paperEmail paper =
  case paper of
    HBL  -> "pren@hbl.fi"
    ON   -> "pren@ostnyland.fi"
    VN   -> "pren@vastranyland.fi"
    KSF  -> paperEmail HBL
