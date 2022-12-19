module Ospedale where

import Prelude

import Data.Either (Either(..), either, isLeft)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import KSF.Navbar.Component (navbar)
import KSF.Paper as Paper
import KSF.Spinner as Spinner
import KSF.User as User
import Lettera as Lettera
import Lettera.Models (Article)
import Ospedale.Page.List as Page.List
import Ospedale.Page.Login as Page.Login
import Ospedale.Routes as Routes
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, useEffectOnce, useState', (/\)) -- )
import React.Basic.Hooks as React

app :: Component {}
app = do
  nav <- Routes.makeInterface
  initialRoute <- either (const Routes.MainPage) identity <<< Routes.match <<< _.path <$> nav.locationState
  articleComponent <- Page.List.component
  React.component "Ospedale" $ const $ React.do
    user /\ setUser <- useState' Nothing
    loading /\ setLoading <- useState' Nothing
    route /\ setRoute <- useState' initialRoute
    paper /\ setPaper <- useState' Nothing
    purgeResult /\ setPurgeResult <- useState' Nothing

    let logout = do
          Aff.launchAff_ $ Spinner.withSpinner setLoading do
            User.logout \logoutResponse -> when (isLeft logoutResponse) $ Console.error "Logout failed"
          liftEffect do
            setUser Nothing
        openArticle :: Article -> Effect Unit
        openArticle article = do
          Routes.setRoute nav $ "/artikel/" <> article.uuid
        newArticle :: Effect Unit
        newArticle = do
          Routes.setRoute nav "/artikel"
        content :: JSX
        content = case unit of
          _ ->
            let list = articleComponent
                  { openArticle
                  , setNavPaper: setPaper <<< Just
                  , newArticle
                  , closeArticle: Routes.setRoute nav "/"
                  , initialArticle: case initialRoute of
                    Routes.ArticlePage u -> u
                    _ -> Nothing
                  }
            in case route of
              Routes.MissingPage -> renderMissing $ Routes.setRoute nav "/"
              _ -> list

    useEffectOnce do
      let attemptMagicLogin :: Aff Unit
          attemptMagicLogin =
            User.magicLogin Nothing \userResponse -> do
              case userResponse of
                Right u -> do
                  setUser $ Just u
                _ -> pure unit
      Aff.launchAff_ attemptMagicLogin

      case initialRoute of
        Routes.ArticlePage _ -> Routes.setRoute nav "/"
        _ -> pure unit

      Routes.routeListener nav $ \r -> do
        setRoute r

    let doPurge purge = do
          setPurgeResult Nothing
          Aff.launchAff_ $ Spinner.withSpinner setLoading do
            liftEffect <<< setPurgeResult <<< Just =<< Lettera.prerenderedCachePurge purge

    pure $ fold
      [ navbar
          { paper: fromMaybe Paper.KSF paper
          , activeUser: user
          , specialHelp: Nothing
          , logout
          }
      , if isJust loading then Spinner.loadingSpinner
        else DOM.div
          { children:
              [ DOM.button
                  { children: [ DOM.text "Purge prerendered frontpage cache" ]
                  , onClick: capture_ $ doPurge Lettera.Purge
                  }
              , DOM.button
                  { children: [ DOM.text "Enable using prerendered frontpage" ]
                  , onClick: capture_ $ doPurge Lettera.Reenable
                  }
              , flip foldMap purgeResult $ \result -> case result of
                    Right _ -> DOM.text "OK"
                    Left err -> DOM.div
                      { className: "error-text"
                      , children: [ DOM.text err ]
                      }
              ]
          }
      , DOM.div
          { className: "ksf-main-container"
          , children: [ content ]
          }
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
