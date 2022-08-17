module Mosaico.Epaper where

import Prelude

import Control.Alt ((<|>))
import Data.Array (mapMaybe)
import Data.Either (Either(..), hush)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe, maybe)
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
import React.Basic.Hooks (Component, useEffect, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { user :: Maybe (Maybe User)
  , paper :: Paper
  , onLogin :: EventHandler
  }

type Credentials =
  { auth :: UserAuth
  , entitlements :: Set Paper
  }

component :: Component Props
component = do
  initialTokens <- loadToken
  React.component "Epaper" $ \{user, paper, onLogin} -> React.do
    -- Nothing when loading
    userEntitlements /\ setUserEntitlements <- useState' $ Just mempty
    userAuth /\ setUserAuth <- useState' initialTokens
    loadingIpAuth /\ setLoadingIpAuth <- useState' true
    ipUserAuth /\ setIpUserAuth <- useState' Nothing
    ipEntitlements /\ setIpEntitlements <- useState' Nothing
    useEffectOnce do
      Aff.launchAff_ do
        tokens <- User.loginIP paper
        case tokens of
          Left _ -> liftEffect $ setLoadingIpAuth false
          Right auth -> do
            liftEffect $ setIpUserAuth $ Just auth
            ipEnt <- User.getUserEntitlements auth
            liftEffect do
              setIpEntitlements $ Just $
                maybe Set.empty (toPaperEntitlements <<< Set.fromFoldable) $
                hush ipEnt
              setLoadingIpAuth false
      pure $ pure unit
    useEffect (_.uuid <$> join user) $ pure (pure unit) <* case join user of
      Nothing -> do
        setUserEntitlements $ Just mempty
        setUserAuth Nothing
      Just _ -> do
        setUserEntitlements Nothing
        tokens <- loadToken
        setUserAuth tokens
        maybe
          (setUserEntitlements $ Just mempty)
          (Aff.launchAff_ <<< (liftEffect <<< setUserEntitlements <<< Just
                               <<< maybe Set.empty toPaperEntitlements <<< hush
                               <=< User.getUserEntitlements)) tokens
    let loading = loadingIpAuth || isNothing user || isNothing userEntitlements
        userCredentials =
          { auth:_, entitlements:_ }
          <$> userAuth
          <*> userEntitlements
        ipCredentials =
          { auth:_, entitlements:_ }
          <$> ipUserAuth
          <*> ipEntitlements
        credentials = ipCredentials <|> userCredentials
    pure $ render onLogin paper loading credentials
  where
    entitlementGiven = [ "hbl-epaper" /\ HBL
                       , "vn-epaper" /\ VN
                       , "on-epaper" /\ ON
                       , "junior-epaper" /\ JUNIOR
                       ]
    toPaperEntitlements :: Set String -> Set Paper
    toPaperEntitlements = Set.mapMaybe (flip lookup entitlementGiven)

render :: EventHandler -> Paper -> Boolean -> Maybe Credentials -> JSX
render onLogin paper loading credentials =
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
                             , renderReadPaper entitled
                             , renderOpen paper onLogin (_.auth <$> credentials) entitled
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
    isEntitledTo p = maybe false (Set.member p <<< _.entitlements) credentials
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
                      else renderOpen JUNIOR onLogin (_.auth <$> credentials) $ isEntitledTo JUNIOR
                    ]
                }
            ]
        }

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

renderReadPaper :: Boolean -> JSX
renderReadPaper entitled  =
  fragment
  [ DOM.p_ [ DOM.text $ "Välkommen till den digitala e-tidningen!" <> entitledDescription ]
  , epaperDescription
  ]
  where
  entitledDescription =
    if entitled
    then " Här får du hela papperstidningen i en digital form. Klicka på en av länkarna nedan för att börja läsa."
    else ""

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
