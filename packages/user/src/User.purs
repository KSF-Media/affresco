module KSF.User where

import Prelude

import KSF.User.Login
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Persona as Persona
import React.Basic as React

type Props =
  { onMerge :: Effect Unit
  , onMergeCancelled :: Effect Unit
  , onRegister :: Effect Unit
  , onRegisterCancelled :: Effect Unit
-- TODO:
--  , onLogin :: Either Error Persona.LoginResponse -> Effect Unit
  , onUserFetch :: Either Error Persona.User -> Effect Unit
  , launchAff_ :: Aff Unit -> Effect Unit
  }

component :: React.Component Props
component = React.createComponent "User"

createUser :: Persona.NewUser -> Aff Persona.LoginResponse
createUser = Persona.register
