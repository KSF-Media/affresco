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
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events as Event
import React.Basic.Hooks (Component, useState, (/\))
import React.Basic.Hooks as React

foreign import icons ::
  { signOut :: String
  , phone :: String
  }

type Props =
  { paper :: Paper
  , specialHelp :: Maybe JSX
  , activeUser :: Maybe User
  , logout :: Effect Unit
  }

component :: Component Props
component = React.component "Navbar" $ \ props -> React.do
  collapsedNavVisibility /\ modifyCollapsedNavVisibility <- useState Hidden
  let collapsedNavJSX = collapsedNav props collapsedNavVisibility modifyCollapsedNavVisibility
  pure $ render props collapsedNavJSX modifyCollapsedNavVisibility

render
  :: Props
  -> JSX
  -> ((Visibility -> Visibility) -> Effect Unit)
  -> JSX
render props collapsedNavJSX modifyCollapsedNavVisibility =
  DOM.div
    { className: "nav--navbars"
    , children:
        [ fullNav props modifyCollapsedNavVisibility
        , hamburgerNav props modifyCollapsedNavVisibility
        , collapsedNavJSX
        ]
    }

-- | Full width navbar
fullNav
  :: Props
  -> ((Visibility -> Visibility) -> Effect Unit)
  -> JSX
fullNav props modifyCollapsedNavVisibility =
  DOM.div
    { className: "nav--nav-container"
    , children:
        [ paperLogo props.paper
        , fromMaybe needHelp props.specialHelp
        , showUser props
        , logoutButton props modifyCollapsedNavVisibility
        ]
    }

showUser
  :: Props
  -> JSX
showUser { activeUser } =
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

logoutButton
  :: Props
  -> ((Visibility -> Visibility) -> Effect Unit)
  -> JSX
logoutButton { activeUser, logout } modifyCollapsedNavVisibility =
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
      logout
      modifyCollapsedNavVisibility (const Hidden)

-- | Narrow navbar with hamburger button
hamburgerNav
  :: Props
  -> ((Visibility -> Visibility) -> Effect Unit)
  -> JSX
hamburgerNav props modifyCollapsedNavVisibility =
  DOM.div
    { className: "nav--hamburger-container"
    , children:
        [ paperLogo props.paper
        , hamburgerButton modifyCollapsedNavVisibility
        ]
    }

-- | Items of hamburger nav
collapsedNav
  :: Props
  -> Visibility
  -> ((Visibility -> Visibility) -> Effect Unit)
  -> JSX
collapsedNav props collapsedNavVisibility modifyCollapsedNavVisibility =
  Collapsed.render
    { visibility: collapsedNavVisibility
    , navItems:
        [ logoutButton props modifyCollapsedNavVisibility
        , needHelp
        ]
    }

paperLogo
  :: Paper
  -> JSX
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

hamburgerButton
  :: ((Visibility -> Visibility) -> Effect Unit)
  -> JSX
hamburgerButton modifyCollapsedNavVisibility =
  DOM.div
    { className: "nav--hamburger-button"
    , onClick: Event.handler_ $ modifyCollapsedNavVisibility negateVisibility
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
