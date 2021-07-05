module Prenumerera.Register where

import Data.Maybe (isNothing)
import Data.Nullable (toMaybe)
import KSF.Api.Package (Package)
import KSF.CountryDropDown
import KSF.User (User)
import KSF.User.Login as Login
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, useState, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { user :: Maybe User
  , setUser :: Maybe User -> Effect Unit
  , package :: Package
  }

type RegisterData
  { existingUser :: Bool
  , email :: Maybe String
  , firstName :: Maybe String
  , lastName :: Maybe String
  , streetAddress :: Maybe String
  , city :: Maybe String
  , countryCode :: Maybe String
  , password :: Maybe String
  , confirmPassword :: Maybe String
  }

initialRegisterData :: Maybe User -> RegisterData
initialRegisterData Nothing =
  { existingUser: false
  , email: Nothing
  , firstName: Nothing
  , lastName: Nothing
  , streetAddress: Nothing
  , city: Nothing
  , countryCode: Nothing
  , password: Nothing
  , confirmPassword: Nothing
  }
initialRegisterData (Just user) =
  { existingUser: true
  , email: Just toMaybe 
  , firstName: toMaybe user.firstName
  , lastName: toMaybe user.lastName
  , streetAddress: _.streetAddress <$> address
  , city: (toMaybe <<< _.city) =<< address
  , countryCode: _.countryCode <$> address
    -- Not displayed if existing account
  , password: Nothing
  , confirmPassword: Nothing
  }

type LoginData
  { email :: Maybe String
  , password :: Maybe String
  }

component :: Component Props
component = do
  login <- Login.login
  React.component "Register" $ \ { user, setUser, package } -> do
    let loginForm =
          login
            { onMerge: pure unit
            , onMergeCancelled: pure unit
            , onRegister: pure unit
            , onRegisterCancelled: pure unit
            , onUserFetch: userFetched
            , onLogin: const $ pure unit
            , disableSocialLogins: mempty
            }
        userFetched (Right user) = setUser $ Just user
        userFetched (Left _) = pure unit
    loginScreen /\ setLoginScreen <- useState' $ isNothing user
--    loginData /\ setLoginData <- useState $ { email: Nothing, password: Nothing }
    registerData /\ setRegisterData <- useState $ initialRegisterData user
    pure $ render package $
      if loginScreen
      then renderLoginScreen login
      else renderRegister registerData setRegisterData

render :: Package -> JSX -> JSX
render content =
  DOM.div
    { className: "container"
    , children:
        [ DOM.h4_ [ DOM.text $ "Din beställning: " <> package.name ]
        , content
        ]
    }

{-
renderLoginScreen :: LoginData -> ((LoginData -> LoginData) -> Effect Unit) -> JSX
renderLoginScreen { email, password } 
-}
