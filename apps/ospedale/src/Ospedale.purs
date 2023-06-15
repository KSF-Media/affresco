module Ospedale where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Unsafe (unsafePerformEffect)
import KSF.Navbar.Component (navbar)
import KSF.Paper as Paper
import KSF.Spinner as Spinner
--import KSF.User as User
import Lettera.Fallback as Lettera
import Lettera.Models (Article)
import Ospedale.Login (recoverSession, removeSession)
import Ospedale.Page.Error (renderError)
import Ospedale.Page.List as Page.List
import Ospedale.Page.Login as Page.Login
import Ospedale.Routes as Routes
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, useEffectOnce, useState', (/\)) -- )
import React.Basic.Hooks as React
import Web.HTML as HTML
import Web.HTML.Window as Window
import Debug

app :: Component {}
app = do
  let cancel = Aff.killFiber $ Exception.error "cancel"
  window <- HTML.window
  localStorage <- Window.localStorage window
  let relogin = pure unit
  nav <- Routes.makeInterface
  initialSession <- recoverSession localStorage
  initialRoute <- either (const Routes.MainPage) identity <<< Routes.match <<< _.path <$> nav.locationState
  articleComponent <- Page.List.component
  loginComponent <- Page.Login.component window localStorage
  React.component "Ospedale" $ const $ React.do
    session /\ setSession <- useState' Nothing
    loading /\ setLoading <- useState' $ (const Spinner.Loading) <$> initialSession
    route /\ setRoute <- useState' initialRoute
    paper /\ setPaper <- useState' Nothing
    purgeResult /\ setPurgeResult <- useState' Nothing

    let logout = do
          removeSession localStorage
          setSession Nothing
        openArticle :: Article -> Effect Unit
        openArticle article = do
          Routes.setRoute nav $ "/artikel/" <> article.uuid
        newArticle :: Effect Unit
        newArticle = do
          Routes.setRoute nav "/artikel"
    useEffectOnce do
      case initialSession of
        Nothing -> pure $ pure unit
        Just ini -> do
          fiber <- Aff.launchAff do
            maybeSession <- ini
            liftEffect do
              setSession maybeSession
              setLoading Nothing
          pure $ Aff.launchAff_ $ cancel fiber

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
          , activeUser: _.email <$> session
          , specialHelp: Nothing
          , logout
          }
      , case {loading, session} of
            {loading: Just _} -> Spinner.loadingSpinner
            {session: Nothing} -> loginComponent {setSession: setSession <<< Just}
            {session: Just s} -> renderSite (Just s.session)
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
