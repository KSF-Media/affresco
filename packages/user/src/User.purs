module KSF.User where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import KSF.User.Login as Login
import Persona as Persona
import React.Basic (JSX)
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

type State =
  { logout :: Effect Unit
  , login :: Effect Unit
  , getUser :: Persona.User

  }

component :: React.Component Props
component = React.createComponent "User"

loginJSComponent = Login.jsComponent

loginComponent :: Login.Props -> JSX
loginComponent props =
  Login.login props

createUser :: Persona.NewUser -> Aff Persona.LoginResponse
createUser = Persona.register
