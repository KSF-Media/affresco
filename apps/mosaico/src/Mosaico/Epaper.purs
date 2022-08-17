module Mosaico.Epaper where

import Prelude

import Data.Either (hush)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe,  maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.UUID as UUID
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
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component, useEffect, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { user :: Maybe (Maybe User)
  , paper :: Paper
  , onLogin :: EventHandler
  }

component :: Component Props
component = do
  initialTokens <- loadToken
  React.component "Epaper" $ \{user, paper, onLogin} -> React.do
    entitlements /\ setEntitlements <- useState' Nothing
    userAuth /\ setUserAuth <- useState' initialTokens
    useEffect (_.uuid <$> join user) do
      when (isNothing $ join user) $ Aff.launchAff_ do
        tokens <- User.loginIP paper
        liftEffect case (hush tokens) of
          Nothing -> setEntitlements $ Nothing
          Just auth -> do
            setUserAuth $ Just auth
            Aff.launchAff_ $ do
              ipEntitlements <- User.getUserEntitlements auth
              liftEffect $ setEntitlements $ hush ipEntitlements
      when (isNothing entitlements && isJust (join user)) do
        setEntitlements Nothing
        tokens <- loadToken
        setUserAuth tokens
        maybe
          (setEntitlements $ Just mempty)
          (Aff.launchAff_ <<< (liftEffect <<< setEntitlements <<< Just
                               <<< fromMaybe Set.empty <<< hush <=< User.getUserEntitlements)) tokens
      pure $ pure unit
    pure $ render onLogin paper (isNothing user) userAuth entitlements

render :: EventHandler -> Paper -> Boolean -> Maybe UserAuth -> Maybe (Set String) -> JSX
render onLogin paper loadingUser userAuth entitlements =
  DOM.div
    { className: "mosaico-epaper"
    , children:
        [ DOM.div
            { className: "mosaico-epaper--main mosaico-epaper--section"
            , children:
                [ DOM.a
                    { className: "mosaico-epaper--teaser"
                    , href: if entitled then latestEpaper paper else "https://prenumerera.ksfmedia.fi/#/" <> Paper.cssName paper
                    , children: [ DOM.img { src: "https://cdn.ksfmedia.fi/mosaico/tablet-bg.png" } ]
                    }
                , DOM.div
                    { className: "mosaico-epaper--body"
                    , children:
                        if loading
                        then [ loadingSpinner ]
                        else [ DOM.h2_ [ DOM.text "Läs dagens tidning" ]
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
    entitled = isEntitledTo paper
    junior =
      DOM.div
        { className: "mosaico-epaper--section"
        , children:
            [ DOM.div
                { className: "mosaico-epaper--teaser mosaico-epaper--teaser__junior"
                , children: [ DOM.img { src: "https://cdn.ksfmedia.fi/mosaico/tablet-bg.png" } ]
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
    loading = loadingUser || isJust userAuth && isNothing entitlements
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

renderOpen :: Paper -> EventHandler -> Maybe UserAuth -> Boolean -> JSX
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
    { onClick: onLogin
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
