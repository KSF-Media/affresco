module Ospedale.Page.Login where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import KSF.Spinner as Spinner
import Ospedale.Login as Login
import Ospedale.TokenResponse (AccessToken)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useEffectOnce, useState', (/\)) -- )
import React.Basic.Hooks as React
import Web.HTML as Web
import Web.HTML.Window as Window

type Props =
  { setUser :: {token :: AccessToken, name :: String} -> Effect Unit
  }

component :: Component Props
component = do
  window <- Web.window
  let cancel = Aff.killFiber $ Exception.error "cancel"
  -- For now, make user login every time
  monitor <- Login.getMonitor
  React.component "OspetaleLogin" $ \ { setUser } -> React.do
    error /\ setError <- useState' Nothing
    token /\ setToken <- useState' Nothing
    useEffectOnce do
      fiber2 <- Aff.launchAff do
        liftEffect <<< setToken <<< Just =<< monitor.monitor
      fiber1 <- Aff.launchAff do
        result <- monitor.result
        liftEffect <<< either (setError <<< Just) setUser =<< monitor.result
      pure $ Aff.launchAff_ do
        cancel fiber1
        cancel fiber2
    pure $ case {error, token} of
      {error: Just err} -> DOM.text $ "Något gick fel med inloggning: " <> err
      {token: Just t} ->
        DOM.button
          { onClick: handler_ $
                       void $ Window.open ("http://localhost:8081/v4/fallback/login?monitor=" <> t) "" "" window
          , children: [ DOM.text "Logga in" ]
          }
      _ -> Spinner.loadingSpinner
{-
    loading /\ setLoading <- useState' Nothing
    let withSpinner :: forall a. Aff a -> Aff a
        withSpinner = Spinner.withSpinner setLoading
        loginForm =
          login
            { onMerge: pure unit
            , onMergeCancelled: pure unit
            , onRegister: pure unit
            , onRegisterCancelled: pure unit
            , onUserFetch: userFetched
            , onLogin: Aff.launchAff_ <<< withSpinner
            , disableSocialLogins: mempty
            , paper: Nothing
            }
        userFetched (Right u) = do
          setUser $ Just u
        userFetched (Left _) = do
          -- TODO show error
          pure unit
    pure $ case loading of
      Just Spinner.Loading -> Spinner.loadingSpinner
      Nothing -> loginForm
-}
