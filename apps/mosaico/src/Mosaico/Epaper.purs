module Mosaico.Epaper where

import Prelude

import Data.Either (hush)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe,  maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Auth (loadToken)
import KSF.Api (UserAuth, Token(..))
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import KSF.User as User
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, useEffect, useState', (/\))
import React.Basic.Hooks as React

-- TODO: Show some loading indicator if trying SSO
type Props =
  { user :: Maybe User
  , entitlements :: Maybe (Set String)
  , paper :: Paper
  , onLogin :: Effect Unit
  }

component :: Component Props
component = do
  initialTokens <- loadToken
  React.component "Epaper" $ \props@{user, paper, onLogin} -> React.do
    entitlements /\ setEntitlements <- useState' props.entitlements
    userAuth /\ setUserAuth <- useState' initialTokens
    -- Used to prevent reloading entitlements if they were in props
    initializing /\ setInitializing <- useState' true
    useEffect (_.uuid <$> user) do
      when (isNothing user) $ setEntitlements Nothing
      when (not initializing || isNothing entitlements && isJust user) do
        setEntitlements Nothing
        tokens <- loadToken
        setUserAuth tokens
        maybe
          (setEntitlements $ Just mempty)
          (Aff.launchAff_ <<< (liftEffect <<< setEntitlements <<< Just
                               <<< fromMaybe Set.empty <<< hush <=< User.getUserEntitlements)) tokens
      setInitializing false
      pure $ pure unit
    pure $ render onLogin paper userAuth entitlements

render :: Effect Unit -> Paper -> Maybe UserAuth -> Maybe (Set String) -> JSX
render onLogin paper userAuth entitlements =
  DOM.div
    { className: "mosaico-epaper"
    , children:
        [ DOM.div
            { className: "mosaico-epaper--main mosaico-epaper--section"
            , children:
                [ DOM.div
                    { className: "mosaico-epaper--teaser"
                    , children:
                        [ DOM.a
                            { href: latestEpaper paper
                            , children:
                                [ DOM.img { src: "https://cdn.ksfmedia.fi/mosaico/tablet.png" } ]
                            }
                        ]
                    }
                , DOM.div
                    { className: "mosaico-epaper--body"
                    , children:
                        if loading
                        then [ loadingSpinner ]
                        else let entitled = isEntitledTo paper
                             in [ DOM.h2_ [ DOM.text "Läs dagens tidning" ]
                                , renderReadPaper userAuth entitled
                                , renderOpen paper onLogin userAuth entitled
                                ]
                    }
                ]
            }
        , case paper of
              HBL -> junior
              _ -> mempty
        ]
    }
  where
    junior =
      DOM.div
        { className: "mosaico-epaper--section"
        , children:
            [ DOM.div
                { className: "mosaico-epaper--teaser mosaico-epaper--teaser__junior"
                }
            , DOM.div
                { className: "mosaico-epaper--body"
                , children:
                    [ juniorDescription
                    , if loading then loadingSpinner
                      else renderOpen JUNIOR onLogin userAuth $ isEntitledTo JUNIOR
                    ]
                }
            ]
        }
    loading = isJust userAuth && isNothing entitlements
    entitlementNeeded = [ HBL /\ "hbl-epaper"
                        , VN /\ "vn-epaper"
                        , ON /\ "on-epaper"
                        , JUNIOR /\ "junior-epaper"
                        ]
    isEntitledTo p = fromMaybe false $
                     Set.member <$> lookup p entitlementNeeded <*> entitlements

epaperSite :: Paper -> String
epaperSite HBL    = "https://etidningen.hbl.fi/"
epaperSite VN     = "https://etidningen.vastranyland.fi/"
epaperSite ON     = "https://etidningen.ostnyland.fi/"
epaperSite JUNIOR = "https://e-junior.hbl.fi/"
epaperSite _      = "INVALID"

latestEpaper :: Paper -> String
latestEpaper paper = epaperSite paper <> case paper of
  HBL    -> "titles/hbl/3271/publications/latest"
  VN     -> "titles/vastranyland/3668/publications/latest"
  ON     -> "titles/ostnyland/3669/publications/latest"
  JUNIOR -> "titles/hbljunior/12509/publications/latest"
  _      -> "INVALID"

authQuery :: UserAuth -> String
authQuery { userId, authToken: (Token token) } = "?uuid=" <> UUID.toString userId <> "&token=" <> token

epaperDescription :: JSX
epaperDescription =
  fragment
  [ DOM.p_ [ DOM.text "I e-tidningen kan du bland annat:" ]
  , DOM.ul_
      [ DOM.li_ [ DOM.text "läsa artiklar i en lättläst textvy" ]
      , DOM.li_ [ DOM.text "söka efter artiklar i tidningen" ]
      , DOM.li_ [ DOM.text "fylla i korsordet digitalt" ]
      ]
  ]

renderOpen :: Paper -> Effect Unit -> Maybe UserAuth -> Boolean -> JSX
renderOpen paper _ (Just tokens) true =
  fragment
    [ DOM.a
      { target: "_blank"
      , className: "mosaico-epaper--primary-button"
      , href: latestEpaper paper <> authQuery tokens
      , children: [ DOM.text "Öppna det senaste numret" ]
      }
  , DOM.a
      { target: "_blank"
      , className: "mosaico-epaper--secondary-button"
      , href: epaperSite paper <> authQuery tokens
      , children: [ DOM.text "Bläddra i arkivet" ]
      }
  ]
renderOpen paper _ (Just _) false =
  fragment
    [ DOM.p_ [ DOM.text "Teckna en prenumeration för full åtkomst" ]
    , DOM.a
        { target: "_blank"
        , className: "mosaico-epaper--primary-button"
        , href: "https://prenumerera.ksfmedia.fi/#/" <> Paper.cssName paper
        , children: [ DOM.text "Prenumerera" ]
        }
    ]
renderOpen _ onLogin Nothing _ =
  DOM.a
    { onClick: capture_ onLogin
    , className: "mosaico-epaper--primary-button"
    , children: [ DOM.text "Logga in" ]
    }

renderReadPaper :: Maybe UserAuth -> Boolean -> JSX
renderReadPaper (Just _) true =
  fragment
  [ DOM.p_ [ DOM.text "Välkommen till den digitala e-tidningen! Här får du hela papperstidningen i en digital form. Klicka på en av länkarna nedan för att börja läsa." ]
  , epaperDescription
  ]
renderReadPaper _ _ =
  fragment
  [ DOM.p_ [ DOM.text "Välkommen till den digitala e-tidningen!" ]
  , epaperDescription
  ]

juniorDescription :: JSX
juniorDescription =
  fragment
  [ DOM.h2_ [ DOM.text "HBL Junior" ]
  , DOM.p_ [ DOM.text "HBL Junior är samhällsmagasinet för barn i åldern 7–13 år. Här kan du läsa tidningen i ett digitalt format. Tidningen utkommer varannan vecka." ]
  , DOM.p_ [ DOM.text "I HBL Junior hittar du det här och mycket mer:" ]
  , DOM.ul_
      [ DOM.li_ [ DOM.text "Nyheter och aktuella ämnen" ]
      , DOM.li_ [ DOM.text "Kultur och nöje" ]
      , DOM.li_ [ DOM.text "Sport och fritid" ]
      ]
  ]
