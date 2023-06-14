module Ospedale.Page.Login where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import KSF.Spinner as Spinner
import Lettera.Fallback (fallbackLoginUrl)
import Ospedale.Login as Login
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useEffectOnce, useState', (/\)) -- )
import React.Basic.Hooks as React
import Web.HTML (Window)
import Web.HTML.Window as Window
import Web.Storage.Storage (Storage)

type Props =
  { setSession :: Login.Session -> Effect Unit
  }

component :: Window -> Storage -> Component Props
component window storage = do
  let cancel = Aff.killFiber $ Exception.error "cancel"
  -- For now, make user login every time
  monitor <- Login.getMonitor storage
  React.component "OspedaleLogin" $ \ { setSession } -> React.do
    error /\ setError <- useState' Nothing
    token /\ setToken <- useState' Nothing
    useEffectOnce do
      fiber2 <- Aff.launchAff do
        liftEffect <<< setToken <<< Just =<< monitor.monitor
      fiber1 <- Aff.launchAff do
        liftEffect <<< either (setError <<< Just) setSession =<< monitor.result
      pure $ Aff.launchAff_ do
        cancel fiber1
        cancel fiber2
    pure $ case {error, token} of
      {error: Just err} -> DOM.text $ "Något gick fel med inloggning: " <> err
      {token: Just t} ->
        DOM.button
          { onClick: handler_ $
                       void $ Window.open (fallbackLoginUrl <> "?monitor=" <> t) "" "" window
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
