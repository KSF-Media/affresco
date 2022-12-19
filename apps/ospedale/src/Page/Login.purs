module Ospedale.Page.Login where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import KSF.User (User)
import KSF.User.Login as Login
import KSF.Spinner as Spinner
import React.Basic.Hooks (Component, useState', (/\)) -- )
import React.Basic.Hooks as React

type Props =
  { setUser :: Maybe User -> Effect Unit
  }

component :: Component Props
component = do
  login <- Login.login
  React.component "OspetaleLogin" $ \ { setUser } -> React.do
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
            }
        userFetched (Right u) = do
          setUser $ Just u
        userFetched (Left _) = do
          -- TODO show error
          pure unit
    pure $ case loading of
      Just Spinner.Loading -> Spinner.loadingSpinner
      Nothing -> loginForm
