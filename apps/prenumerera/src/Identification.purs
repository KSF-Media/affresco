module Prenumerera.Identification where

import Prelude

import Affjax.Web (printError)
import Bottega (IdentificationError(..))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import KSF.Spinner as Spinner
import KSF.Window (clearOpener)
import KSF.User (User)
import Persona.Identification as Identification
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Web.HTML.Window as Window

-- The order will still be done with the data the customer inputs.
-- This is just for storing it to our own systems, if we need it.

type Props =
  { user :: User
  , next :: Effect Unit
  , setError :: IdentificationError -> Effect Unit
  }

component :: Window.Window -> Component Props
component window = do
  let cancel = Aff.killFiber $ Exception.error "cancel"
  React.component "Identification" $ \{ user, next, setError } -> React.do
    monitorStarted /\ setMonitorStarted <- useState' false

    useEffectOnce do
      fiber <- Aff.launchAff do
        -- Set a HTTP only cookie for the monitor
        tokenResponse <- Identification.getToken
        case tokenResponse of
          Left err -> liftEffect $ setError $ StrongIdentificationFailed $ printError err
          Right _ -> do
            mon <- liftEffect $ Identification.getMonitor user
            mon.monitor
            maybeWin <- liftEffect do
              setMonitorStarted true
              Window.open Identification.loginURL "_blank" "" window
            case maybeWin of
              Nothing -> liftEffect $ setError StrongIdentificationWindowOpenFailed
              Just w -> do
                liftEffect $ clearOpener w
                res <- mon.result
                liftEffect do
                  Window.close w
                  case res of
                    Right _ -> next
                    Left err -> setError $ StrongIdentificationFailed err
      pure $ Aff.launchAff_ $ cancel fiber

    pure $ if monitorStarted then mempty else fold
      [ DOM.text "Vänligen vänta, kundidentifikation skall öppnas"
      , Spinner.loadingSpinner
      ]
