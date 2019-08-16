module KSF.Navbar.View where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.Events as Event
import React.Basic (JSX)

-- foreign import icons ::
--   { signOut :: String
--   , phone :: String
--   }

type PaperInfo =
  { logo :: String
  , email :: String
  , phoneNumber :: String
  }

type Attributes =
  { onLogout :: Maybe (Effect Unit)
  , toggleCollapsedNav :: Effect Unit
  , paperInfo :: PaperInfo
  , collapsedNav :: Array JSX -> JSX
  }

navbar :: Attributes -> JSX
navbar { onLogout, toggleCollapsedNav, paperInfo, collapsedNav } =
  DOM.div
    { className: "nav--nav-container"
    , children:
        [ DOM.div
            { className: "nav--navbar clearfix pt2 pb2"
            , children:
                [ DOM.div
                    { className: "nav--logo-container col col-3 pb2 pt1"
                    , children:
                        [ DOM.img
                            { className: "nav--paper-logo"
                            , src: paperInfo.logo
                            }
                        ]
                    }
                , DOM.div
                    { className: "nav--right-container col-9 pt1 flex justify-center"
                    , children:
                        [ DOM.div
                            { className: "nav--right-uncollapsed-content flex"
                            , children:
                                [ customerService
                                , logoutButton
                                ]
                            }
                        , DOM.div_ [ hamburgerButton ]
                        ]
                    }
                ]
            }
        , collapsedNav [ logoutButton, customerService ]
        ]
    }
  where
    logoutButton = foldMap signOutButton onLogout

    collapsedMenuRow :: Array JSX -> JSX
    collapsedMenuRow children =
      DOM.div
        { className: "nav--collapsed-menu-row flex items-center ml2 pb1"
        , children
        }

    customerService :: JSX
    customerService =
      DOM.div
        { className: "nav--customer-service-container flex items-center mr3"
        , children:
            [ DOM.div
                { className: "nav--contact-info ml1"
                , children:
                    [ DOM.div
                        { className: "nav--need-help"
                        , children: [ DOM.strong_ [ DOM.text "Behöver du hjälp?" ] ]
                        }
                    , DOM.div_ [ formatMailtoAnchorTag paperInfo.email ]
                    ]
                }
            ]
        }

    signOutButton :: (Effect Unit) -> JSX
    signOutButton logout =
      DOM.div
        { className: "flex items-center nav--sign-out-container"
        , onClick: Event.handler_ logout
        , children:
            [ DOM.img
                { className: "nav--sign-out-symbol"
             --   , src: icons.signOut
                }
            , DOM.div
                { className: "nav--sign-out ml1"
                , children: [ DOM.text "Logga ut" ]
                }
            ]
        }

    hamburgerButton :: JSX
    hamburgerButton =
      DOM.div
        { className: "col mr3 nav--hamburger-button"
        , onClick: Event.handler_ toggleCollapsedNav
        , children:
            [ hamburgerStripe
            , hamburgerStripe
            , hamburgerStripe
            ]
        }
      where
        hamburgerStripe = DOM.div { className: "nav--hamburger-stripe" }

    formatTelAnchorTag :: String -> JSX
    formatTelAnchorTag phoneNumber =
      DOM.a
        { href: phoneNumberLink
        , children: [ DOM.text phoneNumber ]
        }
      where
        phoneNumberLink = "tel:" <> phoneNumWithAreaCode
        phoneNumWithAreaCode =
          case String.uncons phoneNumber of
            Just { tail } -> "+358 " <> tail
            Nothing -> phoneNumber

    formatMailtoAnchorTag :: String -> JSX
    formatMailtoAnchorTag email =
      DOM.a
        { href: "mailto:" <> email
        , children: [ DOM.text email ]
        }

    formatLanguageSelect :: JSX
    formatLanguageSelect =
      DOM.select
        { className: ""
        , children:
            [ DOM.option { value: "sv", children: [ DOM.text "Svenska" ] }
            , DOM.option { value: "fi", children: [ DOM.text "Suomeksi" ] }
            ]
        }
