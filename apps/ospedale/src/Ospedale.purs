module Ospedale where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import KSF.Navbar.Component (navbar)
import KSF.Paper as Paper
import KSF.Spinner as Spinner
--import KSF.User as User
import Lettera.Fallback as Lettera
import Lettera.Models (Article)
import Ospedale.Page.Error (renderError)
import Ospedale.Page.List as Page.List
import Ospedale.Page.Login as Page.Login
import Ospedale.Routes as Routes
import Ospedale.TokenResponse (AccessToken, listenAccessToken)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, useEffectOnce, useState', (/\)) -- )
import React.Basic.Hooks as React
import Debug

app :: Component {}
app = do
  let relogin = pure unit
  nav <- Routes.makeInterface
  initialRoute <- either (const Routes.MainPage) identity <<< Routes.match <<< _.path <$> nav.locationState
  articleComponent <- Page.List.component
  loginComponent <- Page.Login.component
  React.component "Ospedale" $ const $ React.do
    token /\ setToken <- useState' Nothing
    let maintainToken :: (Maybe String -> Effect Unit) -> (Maybe {name :: String, token :: AccessToken}) -> Effect Unit
        maintainToken set newUser = do
          foldMap (listenAccessToken setToken <<< _.token) newUser
          set (_.name <$> newUser)
    user /\ setUser <- (map <<< map) maintainToken $ useState' Nothing
    loading /\ setLoading <- useState' Nothing
    route /\ setRoute <- useState' initialRoute
    paper /\ setPaper <- useState' Nothing
    purgeResult /\ setPurgeResult <- useState' Nothing

    let logout = do
          -- TODO
          setUser Nothing
        openArticle :: Article -> Effect Unit
        openArticle article = do
          Routes.setRoute nav $ "/artikel/" <> article.uuid
        newArticle :: Effect Unit
        newArticle = do
          Routes.setRoute nav "/artikel"
    useEffectOnce do
      case initialRoute of
        Routes.ArticlePage _ -> Routes.setRoute nav "/"
        _ -> pure unit

      Routes.routeListener nav $ \r -> do
        setRoute r

    let doPurge purge tok = do
          setPurgeResult Nothing
          Aff.launchAff_ $ Spinner.withSpinner setLoading do
            liftEffect <<< setPurgeResult <<< Just =<< Lettera.prerenderedCachePurge tok purge

    let renderSite accessToken = fold
          [ DOM.div
              { children:
                  [ DOM.button
                      { children: [ DOM.text "Purge prerendered frontpage cache" ]
                      , onClick: foldMap (capture_ <<< doPurge Lettera.Purge) accessToken
                      }
                  , DOM.button
                      { children: [ DOM.text "Enable using prerendered frontpage" ]
                      , onClick: foldMap (capture_ <<< doPurge Lettera.Reenable) accessToken
                      }
                  , flip foldMap purgeResult $ \result -> case result of
                        Right _ -> DOM.text "OK"
                        Left err -> renderError relogin "Logga in igen" err
                  ]
              }
          , DOM.div
              { className: "ksf-main-container"
              , children: pure $
                  let list = articleComponent
                        { openArticle
                        , setNavPaper: setPaper <<< Just
                        , newArticle
                        , closeArticle: Routes.setRoute nav "/"
                        , initialArticle:
                          case initialRoute of
                            Routes.ArticlePage a -> a
                            _ -> Nothing
                        , relogin
                        , token: accessToken
                        }
                  in case route of
                    Routes.MissingPage -> renderMissing $ Routes.setRoute nav "/"
                    _ -> list
              }
          ]

    pure $ fold
      [ navbar
          { paper: fromMaybe Paper.KSF paper
          , activeUser: user
          , specialHelp: Nothing
          , logout
          }
      , case {loading, user, token} of
            {loading: Just _} -> Spinner.loadingSpinner
            {user: Nothing} -> loginComponent {setUser: setUser <<< Just}
            -- Session expired or renew failed
            {user: Just _, token: Nothing} ->
              DOM.div
                { className: "ksf-session-expired-overlay"
                , children:
                    [ DOM.div_
                        [ DOM.h2_ [ DOM.text "Session expired" ]
                        , DOM.text "Click to log in again"
                        , DOM.button
                            { children: [ DOM.text "Relogin" ]
                            , onClick: capture_ $ setUser Nothing
                            }
                        ]
                    ]
                } <> renderSite Nothing
            {token: Just tok} -> renderSite (Just tok)
      ]

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

renderMissing :: Effect Unit -> JSX
renderMissing reset =
  DOM.div
    { children:
        [ DOM.text "Något gick fel. "
        , DOM.a
            { href: "/"
            , onClick: capture_ reset
            , children: [ DOM.text "Försök igen." ]
            }
        ]
    }
